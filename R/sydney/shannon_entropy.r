# function to compute entropy
shannon.entropy <- function(p)
{
  if (any(is.na(p)) || any(is.nan(p)))
    return(NA)
  if (min(p) < 0 || sum(p) <= 0)
    return(NA)
  p.norm <- p[p>0]/sum(p)
  -sum(log2(p.norm)*p.norm)
}