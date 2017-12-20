package io.github.yannick_cw.http4s_cache

import cats.data.{Kleisli, OptionT}
import cats.effect.Effect
import cats.implicits._
import org.http4s.dsl.impl.Methods
import org.http4s.{HttpService, Request, Response}

import scala.concurrent.duration.Duration
import scalacache.{Cache, Mode}

object CachingMiddleware {

  def constTtl[F[_], K](cache: Cache[Option[Response[F]]], keyer: PartialFunction[Request[F], F[K]], ttl: Duration)(
      service: HttpService[F])(implicit mode: Mode[F], F: Effect[F]): HttpService[F] =
    dynamicTtl(cache, keyer.andThen(_.map((_, Option(ttl)))))(service)

  def infiniteTtl[F[_], K](cache: Cache[Option[Response[F]]], keyer: PartialFunction[Request[F], F[K]])(
      service: HttpService[F])(implicit mode: Mode[F], F: Effect[F]): HttpService[F] =
    dynamicTtl(cache, keyer.andThen(_.map((_, None: Option[Duration]))))(service)

  def dynamicTtl[F[_], K](cache: Cache[Option[Response[F]]],
                          keyerWithTtl: PartialFunction[Request[F], F[(K, Option[Duration])]])(
      service: HttpService[F])(implicit mode: Mode[F], F: Effect[F]): HttpService[F] =
    Kleisli(
      req =>
        OptionT(
          keyerWithTtl
            .lift(req)
            .fold(service(req).value)(_.flatMap {
              case (key, ttl) => cache.cachingF(key)(ttl)(service(req).value)
            })))
}

object Keyer extends Methods {

  def getKeyer[F[_]](implicit F: Effect[F]): PartialFunction[Request[F], F[String]] = {
    case req if req.method == GET => F.pure(req.uri.renderString)
  }

  def postKeyer[F[_]: Effect]: PartialFunction[Request[F], F[String]] = {
    case req if req.method == POST => req.as[String].map(s => s ++ req.uri.renderString)
  }
}
