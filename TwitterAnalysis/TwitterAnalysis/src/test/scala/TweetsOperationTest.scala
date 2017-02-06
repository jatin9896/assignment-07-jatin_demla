import org.scalatest.FunSuite

import scala.concurrent.Await
import scala.concurrent.duration._

class TweetsOperationTest extends FunSuite {
  val tweets = new TweetsOperation
  test("should result list of tweets") {
    assert(Await.result(tweets.queryResult("scala", 100), 10.seconds).size >= 0)
  }
  test("should have tuple of average retweets and like") {
    assert(Await.result(tweets.avgLikesAndRetweet("scala", 100), 10.second) != (0, 0))
  }
}
