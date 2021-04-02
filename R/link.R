link <- function( x, link_type = c("identity", "logit") )
{
  list( identity = function(x){x},
        logit = function(x){ 1/( 1 + exp(-x)) })[[link_type]](x)
}
