package tradex.domain.ext.http4s

import tradex.domain.ext.derevo.Derive

import org.http4s.QueryParamDecoder

object queryParam extends Derive[QueryParamDecoder]
