import java.util.{Locale, ResourceBundle}

import org.apache.log4j.Logger
import twitter4j._
import twitter4j.conf.ConfigurationBuilder

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TweetsOperation {
  val logger = Logger.getLogger(this.getClass)

  /*
  *  this method return a tupple having average retweet in first field and average like in next field
   */
  def avgLikesAndRetweet(hashtag: String, tweetcount: Int): Future[(Future[Int], Future[Int])] = {
    val output = queryResult(hashtag, tweetcount)
    val retweet = output map (statuslist => statuslist.map(status => status.getRetweetCount).sum)
    val likes = output map (statuslist => statuslist.map(status => status.getFavoriteCount).sum)
    val avgtupple = for {
      rtweet <- retweet
      like <- likes
      avgretweet = output.map(rtweet / _.size)
      avglike = output.map(like / _.size)
    } yield (avgretweet, avglike)
    avgtupple
  }

  /*
  *  this method provide list of tweets status on basis of searching with a keyword
   */
  def queryResult(hashtag: String, tweetcount: Int): Future[List[Status]] = {
    val twitter = getConnection()
    Future {
      val query = new Query("#" + hashtag)
      query.count(tweetcount)
      query.since("12-11-2011")
      val queryresult = twitter.search(query)
      val status = queryresult.getTweets.asScala.toList
      printTweets(status)
      status
    }
  }

  /*
  * this method is connection factory for twitter connection
   */
  private def getConnection(): Twitter = {
    try {
      val property = ResourceBundle.getBundle("twitterconfig", Locale.getDefault)
      val configurationBuilder = new ConfigurationBuilder
      configurationBuilder.setDebugEnabled(true)
        .setOAuthConsumerKey(property.getString("consumerkey"))
        .setOAuthConsumerSecret(property.getString("consumersecret"))
        .setOAuthAccessToken(property.getString("accesstoken"))
        .setOAuthAccessTokenSecret(property.getString("accesstokensecret"))
      val twitter = new TwitterFactory(configurationBuilder.build()).getInstance()
      logger.debug("Connection Established")
      twitter
    }
    catch {
      case ex: TwitterException => throw new TwitterException("Connection failed")
    }
  }

  /*
  * this method print feeds of twitter
   */
  private def printTweets(status: List[Status]) = status foreach (x => logger.debug("User Name : " + x.getUser.getName + "\t" + "Status : " + x.getText))

}
