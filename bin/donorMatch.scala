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
import org.scribe.model.Token
import org.scribe.model.OAuthRequest
import org.scribe.model.Verb
import org.scribe.model.Response
import com.dragade.scalali._
import java.util.Scanner
import scala.xml.XML
import scala.collection.mutable.HashMap

if (args.length != 3) {
  println("USAGE: donorMatch <firstName> <lastName> <zipCode>")
  System.exit(0)
}

val firstName = args(0)
val lastName = args(1)
val zipCode = args(2)
println("Looking for %s %s living in %s".format(firstName,lastName,zipCode))

val apiKey = "v10c6rivp1cm"
val secretKey = "bSDexg7IRLSQHCJy"

val scalali = new ScalaLi(apiKey, secretKey)
val (url, requestToken) = scalali.initialize()

println("Go hit URL\nhttps://www.linkedin.com/uas/oauth/authenticate?oauth_token=%s".format(requestToken.token))
print("Verifier: ")
val verifier = (new Scanner(System.in)).next
println("\nUsing verifier " + verifier)

val accessToken : AccessToken = scalali.verify(requestToken,verifier)
val oauthService = scalali.oauthService

//execute a people search for this person and output all the results
val restUrl =
  "http://api.linkedin.com/v1/people-search:" +
  "(people:(id,first-name,last-name,headline,three-current-positions),num-results)" +
  "?first-name=%s&last-name=%s&sort=distance&country-code=us&postal-code=%s".format(firstName,lastName,zipCode)

val orequest: OAuthRequest = new OAuthRequest(Verb.GET, restUrl)
oauthService.signRequest(new Token(accessToken.token,accessToken.secret), orequest)

//actually send the request and get the xml body back
val oresponse: Response = orequest.send();
val body = oresponse.getBody();
val xml = XML.loadString(body)
val people = parsePeopleXml(xml)

val matchMap = findMatchingCompanies(people)
def companyMatches(c : String) = matchMap.contains(c) && matchMap.get(c) == Some(true)

val (lucky, unlucky) = people.partition(!_.companies.filter(companyMatches).isEmpty)

println("The following people have companies that match:")
lucky.foreach( p => {
  val matchingCompanies = p.companies.filter(companyMatches).mkString(",")
  println("%s %s\t\t\t%s".format(p.firstName, p.lastName, matchingCompanies))
})

println("----------------")

println("The following people have companies that DO NOT match:")
unlucky.foreach( p => {
  val companies = if (p.companies.isEmpty) { "N/A" } else { p.companies.mkString(",") }
  println("%s %s\t\t\t%s".format(p.firstName, p.lastName, companies))
})


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
def findMatchingCompanies(people: Seq[Person]) : HashMap[String,Boolean] = {
  val matchMap = new HashMap[String,Boolean]
  val companies = people.flatMap(_.companies)
  companies.foreach(c => {
    if (! matchMap.contains(c)) {
      val matches = queryIfCompanyMatches(c)
      matchMap.put(c,matches)
    }
  })

  matchMap
}

/**
 * Makes the request to the HEP database
 */
def queryIfCompanyMatches(companyName:String) : Boolean = {
  companyName.toLowerCase.startsWith("t")
}