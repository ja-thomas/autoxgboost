# Feature hashing for a set of categorical features
# hash size can be user specified or is automatically set by hash.size()
cpoFeatureHashing = makeCPO("featurehashing",  # nolint
  pSS(hash.size = 0: integer [0, ]),
  dataformat = "factor",
  properties.needed = "numerics",
  properties.adding = c("factors", "ordered"),
  packages = "FeatureHashing",
  cpo.train = {
    if(hash.size == 0)
      FeatureHashing::hash.size(data)
    else
      hash.size
  },
  cpo.retrafo = {
    data.frame(as.matrix(FeatureHashing::hashed.model.matrix(~., data, hash.size = control)))
  }
)


# Extract information
cpoExtractTimeStampInformation = makeCPO("extract.timestamp.info",
  dataformat = "numeric",
  properties.needed = "numerics",
  packages = c("lubridate", "anytime"),
  cpo.train = NULL,
  cpo.retrafo =  {
    res = lapply(data, function(col.ts) {
      col = anytime::anytime(col.ts)
      data.frame(
        timestamp = col.ts,
        year = lubridate::year(col),
        month = lubridate::month(col),
        day_of_month = lubridate::mday(col),
        day_of_week = lubridate::wday(col),
        hour_of_day = lubridate::hour(col),
        minute_of_hour = lubridate::minute(col),
        second_of_minute = lubridate::second(col)
        )
    })
    data.frame(do.call(cbind, res))
  }
)


