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
import java.io.File
import java.io.PrintWriter

if (args.length < 3) {
  println("USAGE: donorMatch <firstName> <lastName> <zipCode> [pageNum]")
  println("  firstName, lastName, and zipCode are required")
  println("  optionally you can pass a starting count. If there are more than 25 people with the given name")
  println("  then a value of 1 would get people 26-50, etc.")
  System.exit(0)
}

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

val PEOPLE_PER_PAGE = 25
val firstName = args(0)
val lastName = args(1)
val zipCode = args(2)
val start = if (args.size == 4) { args(3).toInt * PEOPLE_PER_PAGE } else { 0 }

println("Looking for %s %s living in %s. start count %d".format(firstName,lastName,zipCode, start))

//execute a people search for this person and output all the results. The max results is 25 unless we paginate
val restUrl =
  "http://api.linkedin.com/v1/people-search:" +
  "(people:(id,first-name,last-name,headline,three-current-positions),num-results)" +
  "?first-name=%s&last-name=%s&sort=distance&country-code=us&postal-code=%s&start=%d&count=%d"
    .format(firstName,lastName,zipCode,start,PEOPLE_PER_PAGE)

val orequest: OAuthRequest = new OAuthRequest(Verb.GET, restUrl)
oauthService.signRequest(new Token(accessToken.token,accessToken.secret), orequest)
println("....making people search request to LinkedIn")
val oresponse: Response = orequest.send();
val body = oresponse.getBody();
println("....parsing XML")
val xml = XML.loadString(body)
val people = parsePeopleXml(xml)
println("....found %d total people with the name %s %s".format(people.size, firstName, lastName))
val matchMap = findMatchingCompanies(people)
def companyMatches(c : String) = matchMap.contains(c) && ! matchMap.get(c).get.isEmpty

if (people.isEmpty) {
  println("\nNo people search results for that name.")
  System.exit(0)
}

val (lucky, unlucky) = people.partition(!_.companies.filter(companyMatches).isEmpty)

if (! lucky.isEmpty){
  println("\n\nThe following people have companies that match:")
  lucky.foreach( p => {
    val matchingCompanies = p.companies.filter(companyMatches)
    val matchingUrls = matchingCompanies.map(matchMap.get(_).get.head) //one url for each company
    println("%s %s\t\t\t%s\t\t%s\n".format(
      p.firstName, p.lastName, matchingCompanies.mkString(", "), matchingUrls.mkString(", ")))
  })
  println("----------------")
}

if (! unlucky.isEmpty) {
  println("\n\nThe following people have companies that DO NOT match:")
  unlucky.foreach( p => {
    val companies = if (p.companies.isEmpty) { "N/A" } else { p.companies.mkString(",") }
    println("%s %s\t\t\t%s\n".format(p.firstName, p.lastName, companies))
  })
}

/**
 * Holds the relevant person information
 */
case class Person(val id:String, val firstName: String, val lastName: String, val headline: Option[String], val companies: Seq[String])

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
    Person(id, firstName, lastName, headline, companies)
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
