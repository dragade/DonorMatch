#!/bin/sh

PPWD=`pwd -P`
LIB_DIR=$PPWD/`dirname $0`/../lib
FILES=`ls $LIB_DIR`

CP=.
for file in $FILES ; do
 CP=$CP:$LIB_DIR/$file ;
done
exec scala -classpath $CP "$0" "$@"
!#

import io.Source
import java.io.{FileWriter, File, PrintWriter}
import java.util.{ArrayList, Scanner}
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpPost
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.message.BasicNameValuePair
import org.apache.http.NameValuePair
import org.scribe.model.Token
import org.scribe.model.OAuthRequest
import org.scribe.model.Verb
import org.scribe.model.Response
import com.dragade.scalali._
import scala.xml.XML
import scala.collection.mutable.HashMap
val PEOPLE_PER_PAGE = 25

if (args.length != 1) {
  println("USAGE: donorMatch <inputFile>")
  println("  inputFile is a file with each line containing text in the format:")
  println("  firstName | lastName | zipCode")
  println("  Optionally any row can be in the format:")
  println("  firstName | lastName | zipCode | pageNumber")
  println("  If pageNumber is left out, it's assumed to be 0, but if you know for a given name that there are")
  println("  too many people with that name, you can specify a page number (assuming 25 people per page)")
  System.exit(0)
}

case class InputRow(firstName:String, lastName:String, zipCode:String, start: Int)

val inputRows = Source.fromFile(args(0)).getLines.toList.map(s => {
  val parts = s.split("\\|").map(_.trim)
  val start = if (parts.size == 4) { parts(3).toInt * PEOPLE_PER_PAGE } else { 0 }
  InputRow(parts(0), parts(1), parts(2), start)
})
println("There are %d names to search for".format(inputRows.size))

val apiKey = "g7omu90wtvwh"
val secretKey = "AfMTHKwRdq8CZLae"
val scalali = new ScalaLi(apiKey, secretKey)

val authCache = new File(".donorMatchAuthCache")
val accessToken: AccessToken = if (!authCache.exists) {
  val pw = new PrintWriter(authCache);
  val (url, requestToken) = scalali.initialize()
  println("Go hit URL\nhttps://www.linkedin.com/uas/oauth/authenticate?oauth_token=%s".format(requestToken.token))
  print("Verifier: ")
  val verifier = (new Scanner(System.in)).next
  println("\nUsing verifier " + verifier)
  val at : AccessToken = scalali.verify(requestToken,verifier)
  println("Saving %s to .donorMatchAuthCache".format(at))
  pw.println(at.token)
  pw.println(at.secret)
  pw.close
  at
}
else {
  val cache = Source.fromFile(authCache).getLines.toList
  val accessTokenToken = cache(0)
  val accessTokenSecret = cache(1)
  val at : AccessToken = AccessToken(accessTokenToken, accessTokenSecret)
  println("Re-using %s from .donorMatchAuthCache".format(at))
  at
}
val oauthService = scalali.oauthService

val sb = new StringBuilder
sb.append(
"""
<html><head>
<title>DonorMatch Report</title>
</head><body>
<table border="2" cellspacing="2" cellpadding="2">
  <thead>
    <tr><td>First Name</td><td>Last Name</td><td>Headline</td><td>Picture</td><td>Companies</td><td>Matching?</td></tr>
  </thead>
  <tbody>
""")

inputRows.map(doOnePersonSearch).foreach(sb.append)
sb.append("</tbody></table>\n</body></html>\n")

val outputFile = new File ("donorMatchReport.html")
val fw = new FileWriter(outputFile)
fw.write(sb.toString)
fw.close

println("Done! See report at " + outputFile.getAbsolutePath)


/**
 * Does a search for one input row and returns an HTML report snippet
 */
def doOnePersonSearch(row:InputRow) : String = {
  println("Looking for " + row)

  val restUrl =
    "http://api.linkedin.com/v1/people-search:" +
    "(people:(id,first-name,last-name,picture-url,headline,three-current-positions),num-results)" +
    "?first-name=%s&last-name=%s&sort=distance&country-code=us&postal-code=%s&start=%d&count=%d"
      .format(row.firstName, row.lastName, row.zipCode, row.start, PEOPLE_PER_PAGE)

  val orequest: OAuthRequest = new OAuthRequest(Verb.GET, restUrl)
  oauthService.signRequest(new Token(accessToken.token,accessToken.secret), orequest)
  println("....making people search request to LinkedIn")
  val oresponse: Response = orequest.send();
  val body = oresponse.getBody();
  println("....loading XML")
  val xml = XML.loadString(body)
  println("....parsing XML for people")
  val people = parsePeopleXml(xml)
  println("....found %d total people with the name %s %s".format(people.size, row.firstName, row.lastName))
  println("....querying HEP data")
  val matchMap = findMatchingCompanies(people)
  println("....got HEP results")

  if (people.isEmpty) { """<tr><td>%s</td><td>%s</td><td colspan="4">No peoplesearch results!</td></tr>\n""".format(row.firstName,row.lastName) }
  else { generateReportForPeople(people, matchMap) }
}

case class CompanyURL(company:String, url:String)

/**
 * Generates the HTML snippet for a given person's results
 */
def generateReportForPeople(people: Seq[Person], matchMap: HashMap[String,List[String]]) : String = {
  def companyMatches(c : String) = matchMap.contains(c) && ! matchMap.get(c).get.isEmpty
  val sb = new StringBuilder
  val (lucky, unlucky) = people.partition(!_.companies.filter(companyMatches).isEmpty)

  if (! lucky.isEmpty){
    lucky.foreach( p => {
      val matchingCompanies = p.companies.filter(companyMatches)
      val matchingUrls = matchingCompanies.map(c => CompanyURL(c, matchMap.get(c).get.head)) //a tuple of the company name and one url for it
      val matchingUrlHrefs = matchingUrls.map(c => "<a href=\"%s\" target=\"_blank\">%s</a>".format(c.url, c.company))
      val pic = if (p.picture.isDefined) { "<img src=\"%s\"/>".format(p.picture.get) } else { "" }

      sb.append(
        "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n"
        .format(p.firstName, p.lastName, p.headline.getOrElse(""), pic , matchingCompanies.mkString(", "), matchingUrlHrefs.mkString(", ")))
    })
  }

  if (! unlucky.isEmpty) {
    unlucky.foreach( p => {
      val companies = if (p.companies.isEmpty) { "N/A" } else { p.companies.mkString(",") }
      val pic = if (p.picture.isDefined) { "<img src=\"%s\"/>".format(p.picture.get) } else { "" }
      sb.append(
        "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>--</td></tr>\n"
        .format(p.firstName, p.lastName, p.headline.getOrElse(""), pic, companies))
    })
  }

  sb.toString
}

/**
 * Holds the relevant person information
 */
case class Person(val id:String, val firstName: String, val lastName: String, val headline: Option[String], val companies: Seq[String], val picture: Option[String])

/**
 * Parses the xml into a sequence of Person
 */
def parsePeopleXml(xml: scala.xml.Elem) : Seq[Person] = {
  val people = xml \\ "person"
  people.map(p => {
    val id = (p \ "id").text
    val firstName = (p \ "first-name").text
    val lastName = (p \ "last-name").text
    val headline = maybe((p \ "headline").text)
    val companies =  (p \\ "name").map(_.text)
    val picture = maybe((p \ "picture-url").text)

    Person(id, firstName, lastName, headline, companies, picture)
  })
}

def maybe(s:String) : Option[String] = if (s == null || s.isEmpty) None else Some(s)

/**
 * returns a map of company name to whether it matches. companies are only queried once
 */
def findMatchingCompanies(people: Seq[Person]) : HashMap[String,List[String]] = {
  val matchMap = new HashMap[String,List[String]]
  val companies = people.flatMap(_.companies)
  companies.foreach(c => {
    if (! matchMap.contains(c)) {
      val matches = queryMatch(c)
      matchMap.put(c,matches)
    }
  })

  matchMap
}

/**
 * Makes a POST to the redcross page on matchinggifts as if it were Chrome on Mac, parses
 * the resulting HTML looking for links to the companyprofile page
 * returns the list of such URLs
 */
def queryMatch(companyName:String) : List[String] = {
  val cnl = companyName.toLowerCase
  val company = if (cnl.startsWith("the")) { cnl.substring(4) } else { cnl }
  println("....querying HEP for matches for " + company);
  val USER_AGENT = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/535.19 (KHTML, like Gecko) Chrome/18.0.1025.151 Safari/535.19"
  val MATCH_URL =  "http://www1.matchinggifts.com/redcross/companyprofile.cfm?"
  val client = new DefaultHttpClient()
  val formparams = new ArrayList[NameValuePair]
  formparams.add(new BasicNameValuePair("INPUT_ORGNAME", company))
  formparams.add(new BasicNameValuePair("INPUT_ORGNAME_required", "You must input a company name"))
  formparams.add(new BasicNameValuePair("eligible", "ALL"))
  val entity = new UrlEncodedFormEntity(formparams, "UTF-8")
  val httppost = new HttpPost("http://www1.matchinggifts.com/redcross/giftdb.cfm")
  httppost.setEntity(entity)
  httppost.setHeader("User-Agent", USER_AGENT)
  val response = client.execute(httppost)
  val hrefs = Source.fromInputStream(response.getEntity.getContent, "UTF-8").getLines.filter(_.indexOf(MATCH_URL) > 0).toList
  val urls = hrefs.map(s => s.substring(s.indexOf("\"") + 1, s.lastIndexOf("\"")))
  urls
}
