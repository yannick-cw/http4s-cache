package io.github.yannick_cw.http4s_cache

import cats.effect.IO
import org.http4s.{HttpService, Method, Request, Response, Uri}
import org.http4s.dsl.Http4sDsl
import org.scalatest.{Matchers, fixture}

import scalacache.CatsEffect.modes._
import scalacache.Cache
import scalacache.caffeine.CaffeineCache

import scala.language.reflectiveCalls
import scala.concurrent.duration._

class CachingMiddlewareSpec extends fixture.FlatSpec with Http4sDsl[IO] with Matchers {

  def cache = CaffeineCache[Option[Response[IO]]]
  override type FixtureParam = Cache[Option[Response[IO]]]
  override protected def withFixture(test: OneArgTest) =
    withFixture(test.toNoArgTest(cache))

  behavior of "CachingMiddleware with in memory caffeine"

  val getReq                    = Request[IO](Method.GET, Uri.uri("/"))
  def postReq(char: Char = 'f') = Request[IO](Method.POST, Uri.uri("/"), body = fs2.Stream.eval(IO(char.toByte)))
  val testServiceGet            = HttpService[IO] { case GET -> Root => Ok() }
  val testServicePost           = HttpService[IO] { case POST -> Root => Ok() }

  def countingService(service: HttpService[IO] = testServiceGet) = new Object {
    var timesCalled = 0
    val testService = service.map { r =>
      timesCalled = timesCalled + 1
      r
    }
  }

  it should "cache GET requests with 'getKeyer'" in { cache =>
    val service = CachingMiddleware.infiniteTtl(cache, Keyer.getKeyer[IO])(testServiceGet)

    service.orNotFound(getReq).unsafeRunSync()
    cache.get("/").unsafeRunSync().flatten shouldBe defined
  }

  it should "cache POST requests with 'postKeyer'" in { cache =>
    val service = CachingMiddleware.infiniteTtl(cache, Keyer.postKeyer[IO])(testServicePost)

    service.orNotFound(postReq()).unsafeRunSync()
    cache.get("f/").unsafeRunSync().flatten shouldBe defined
  }

  it should "run the 'HttpService' for different POST bodies" in { cache =>
    val ts = countingService(testServicePost)

    val service = CachingMiddleware.infiniteTtl(cache, Keyer.postKeyer[IO])(ts.testService)

    service.orNotFound(postReq()).unsafeRunSync()
    service.orNotFound(postReq('e')).unsafeRunSync()
    ts.timesCalled shouldBe 2
  }

  it should "not run the 'HttpService' for cached Responses" in { cache =>
    val ts      = countingService()
    val service = CachingMiddleware.infiniteTtl(cache, Keyer.getKeyer[IO])(ts.testService)

    service.orNotFound(getReq).unsafeRunSync()
    service.orNotFound(getReq).unsafeRunSync()
    ts.timesCalled shouldBe 1
  }

  it should "not run the 'HttpService' for cached Responses with ttl" in { cache =>
    val ts      = countingService()
    val service = CachingMiddleware.constTtl(cache, Keyer.getKeyer[IO], 1.second)(ts.testService)

    service.orNotFound(getReq).unsafeRunSync()
    service.orNotFound(getReq).unsafeRunSync()
    ts.timesCalled shouldBe 1
  }

  it should "run the 'HttpService' if the constant ttl expired" in { cache =>
    val ts      = countingService()
    val service = CachingMiddleware.constTtl(cache, Keyer.getKeyer[IO], 1.nano)(ts.testService)

    service.orNotFound(getReq).unsafeRunSync()
    Thread.sleep(0, 1)
    service.orNotFound(getReq).unsafeRunSync()
    ts.timesCalled shouldBe 2
  }

  it should "run the 'HttpService' if the dynamic ttl expired" in { cache =>
    val ts = countingService()
    val keyer: PartialFunction[Request[IO], IO[(String, Option[Duration])]] =
      Keyer.getKeyer[IO].andThen(_.map((_, Some(1.nano))))
    val service = CachingMiddleware.dynamicTtl(cache, keyer)(ts.testService)

    service.orNotFound(getReq).unsafeRunSync()
    Thread.sleep(0, 1)
    service.orNotFound(getReq).unsafeRunSync()
    ts.timesCalled shouldBe 2
  }

}
