% Part of the Rmetrics project, http://R-metrics.org/
% Copyright 2005-2011 Rmetrics Core Team
% Distributed under GPL 2 or later

\name{fBasics-deprecated}
%------ NOTE:  ../R/fBasics-deprecated.R   must be synchronized with this!
\alias{fBasics-deprecated}
%% \alias{dstable}
%% \alias{pstable}
%% \alias{qstable}
%% \alias{rstable}
%% \alias{stableMode}

\alias{listDescription}

% functions with names starting with '.' still used by other packages;
% (see file NAMESPACE)
\alias{.acfPlot}
\alias{.contourPlot}
\alias{.distCheck}
\alias{.firePlot}
\alias{.mrlPlot}
\alias{.pacfPlot}
\alias{.perspPlot}
\alias{.plot}
\alias{.predict}
\alias{.qStableFit}
\alias{.residualsPlot}
\alias{.responsesPlot}
\alias{.sliderMenu}
\alias{.unirootNA}


\title{Deprecated functions in package fBasics}

\description{
  These functions are provided for compatibility with older versions of
  the package only, and may be defunct as soon as of the next release.
}

%\usage{
%}
%\arguments{
%}

\details{

  \describe{

    \item{listDescription}{use \code{\link{packageDescription}} instead.}

  }
  
  % There are none currently.

  \code{\link[stabledist]{dstable}}, etc., now are defunct, as they have
  been available from \pkg{stabledist} since early 2011.
  %% dstable(x, alpha, beta, gamma = 1, delta = 0, pm = 0)
  %% pstable(q, alpha, beta, gamma = 1, delta = 0, pm = 0)
  %% qstable(p, alpha, beta, gamma = 1, delta = 0, pm = 0)
  %% rstable(n, alpha, beta, gamma = 1, delta = 0, pm = 0)
  %% stableMode(alpha, beta)
}

\seealso{
  \code{\link{Deprecated}}, \code{\link{Defunct}}
}
\keyword{misc}
