#' Take a subset of claims with the highest fraud scores
#'
#' @param x Object of type BiRankFr.
#' @param Nodes data.table with information on the nodes.
#' @param BeginDate Specifies from which date on you want to include claims.
#' @param NrClaims The number of claims
#' @param ExcludeInvestClaims Logical, exclude investigated claims.
#'
#' @return A list with the following components:
#' @return \item{ResultsClaims}{The dataframe with the fraud scores.}
#' @return  \item{ClaimSubset}{A subset of the Nodes data.table.}
SubSetClaims.BiRankFr <- function(x, Nodes, BeginDate, NrClaims = 10L,
                                  ExcludeInvestClaims = T) {
  if(class(x) != "BiRankFr")
    stop("Provide object of class BiRankFr.")
  if(!all(x$ResultsClaims$ID %in% Nodes$node.id.claim))
    warning("Not all claim IDs found in Nodes database!!!", immediate. = T)
  if(!is.Date(BeginDate))
    stop("Please provide an object of type Date.")
  if(!is.integer(NrClaims))
    stop("Please provide an object of type integer.")

  if(!exists("eso_vec", envir = .GlobalEnv))
    eso_vec <- c("ClaimOutsideCoveragePeriod",
                 "ExaggerationDamage",
                 "FalseDeclarationClaim",
                 "FalseDeclarationSubscription",
                 "FictitiousClaim",
                 "IntentionalDamage",
                 "MoneyLaundering",
                 "MultipleInsurers")
  Nodes[, ':=' (
    FraudInd =
      if(unique(label) == "claim") {
        as.numeric(Motivation_def %in% eso_vec)
      } else {
        rep(0, .N)
      },
    Date =
      if(unique(label) == "claim") {
        as.Date(DSUV, format = '%d/%m/%Y')
      } else {
        rep(as.Date("01/01/1970", format = '%d/%m/%Y'), .N)
      },
    Type =
      if(unique(label) == "claim") {
        claim_type
      } else {
        rep("", .N)
      }
  ), by = label]

  Claims = Nodes[Nodes$label == "claim"]
  NewCl  = Claims[Claims$Date >= BeginDate, get("technical_id")]
  Claims = Claims[Claims$technical_id %in% NewCl]
  if(ExcludeInvestClaims)
    Claims = Claims[Claims$Investigated_by_eso == 0]

  ResultsCl = x$ResultsClaims
  ResultsCl = ResultsCl[which(ResultsCl$ID %in% Claims$node.id.claim), ]

  ResultsCl = ResultsCl[seq_len(NrClaims), ]
  Claims    = Claims[Claims$node.id.claim %in% ResultsCl$ID]

  Results = list(ResultsClaims = ResultsCl,
                 ClaimsSubset  = Claims)
  class(Results) = "BiRankFrSubSetClaims"
  return(Results)
}

#' Take a subset of parties with the highest scores
#'
#' @param x Object of type BiRankFr.
#' @param Nodes data.table with information on the nodes.
#' @param NrParties Number of parties that you want to subset.
#'
#' @return A list with the following components:
#' @return \item{ResultsParties}{The dataframe with the scores for the parties.}
#' @return  \item{PartiesSubset}{A subset of the Nodes data.table.}
SubSetParties.BiRankFr <- function(x, Nodes, NrParties = 10L) {
  if(class(x) != "BiRankFr")
    stop("Provide object of class BiRankFr.")
  if(!all(x$ResultsParties$ID %in% Nodes$node.id.nonclaim))
    warning("Not all claim IDs found in Nodes database!!!", immediate. = T)
  if(!is.integer(NrParties))
    stop("Please provide an object of type integer.")

  RPA = x$ResultsParties
  RPA = RPA[seq_len(NrParties), ]
  IDP = RPA$ID

  PartiesSubSet = Nodes[Nodes$node.id.nonclaim %in% IDP]

  Results = list(ResultsParties = RPA,
                 PartiesSubset  = PartiesSubset)
  class(Results) = "BiRankFrSubSetParties"
  return(Results)
}
