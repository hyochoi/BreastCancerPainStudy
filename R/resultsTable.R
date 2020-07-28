#'
#' @export
get_resultsTable_base = function(formula,data) {
  require(sjPlot)
  formula.bin = as.formula(paste(sub("Pain",replacement="bin.Pain",as.character(formula))[c(2,3)],collapse=" ~ "))
  m.slr = lm(formula, data = data)
  m.glm.nb = glm.nb(formula, data = data)
  m.glm.bin = glm(formula.bin, data = data, family="binomial")
  m.zinb = zeroinfl(formula, data = data, dist="negbin")
  m.hurdle = hurdle(formula, data = data,dist="negbin")

  tab_model(m.slr,m.glm.nb,m.glm.bin,m.zinb,m.hurdle, digits = 3,
            dv.labels = c("SLR","GLM-NB","GLM-BIN","ZINB","Hurdle"),
            title = "Results Table",
            transform="exp",
            show.zeroinf=TRUE, show.intercept=TRUE, show.se=FALSE, show.ci=FALSE, collapse.se=FALSE,
            show.p = TRUE, p.style="both")
}

#'
#' @export
get_resultsTable4painTypes = function(formula,data,model=c("zeroinfl.nb","zeroinfl.poi","lm","glm.nb","glm.poi","hurdle.nb","hurdle.poi")) {
  require(sjPlot)
  require(pscl)

  model = match.arg(model,c("zeroinfl.nb","zeroinfl.poi","lm","glm.nb","glm.poi","hurdle.nb","hurdle.poi"))
  painTypes = c("mean","Q1st","Q2nd","Q3rd","max")
  result = as.list(NULL)
  for (i in seq_along(painTypes)) {
    pain.type = painTypes[i]
    newdata = data
    colnames(newdata)[which(grepl(pattern=pain.type,colnames(newdata))==T)] = c("Pain","Pain.bfchemo","Pain.afchemo")
    if (model=="zeroinfl.nb") {
      result[[i]] = zeroinfl(formula, data = newdata, dist="negbin")
    } else if (model=="zeroinfl.poi") {
      result[[i]] = zeroinfl(formula, data = newdata, dist="poisson")
    } else if (model=="lm") {
      result[[i]] = lm(formula, data = newdata)
    } else if (model=="glm.nb") {
      result[[i]] = glm.nb(formula, data = newdata)
    } else if (model=="glm.poi") {
      result[[i]] = glm(formula, data = newdata, family = "poisson")
    } else if (model=="hurdle.nb") {
      result[[i]] = hurdle(formula, data = newdata, dist="negbin")
    } else if (model=="hurdle.poi") {
      result[[i]] = hurdle(formula, data = newdata, dist="poisson")
    }
  }
  ## Write table
  tab_model(result, digits = 3,
            dv.labels = painTypes,
            title = model,
            transform="exp",
            show.zeroinf=TRUE, show.intercept=TRUE, show.se=FALSE, show.ci=FALSE, collapse.se=FALSE,
            show.p = TRUE, p.style="both")
}

#'
#' @export
get_resultsTable4full = function(formula,data,
                                 pain.type=c("mean","Q1st","Q2nd","Q3rd","max"),
                                 model=c("zeroinfl.nb","zeroinfl.poi","lm","glm.bin","glm.nb","glm.poi","hurdle.nb","hurdle.poi")) {
  require(sjPlot)
  require(pscl)

  newdata = data
  pain.type = match.arg(pain.type,c("mean","Q1st","Q2nd","Q3rd","max"))
  model = match.arg(model,c("zeroinfl.nb","zeroinfl.poi","lm","glm.bin","glm.nb","glm.poi","hurdle.nb","hurdle.poi"))
  colnames(newdata)[which(grepl(pattern=pain.type,colnames(newdata))==T)] = c("Pain","Pain.bfchemo","Pain.afchemo",
                                                                              "Pain.bfsurgery","Pain.afsurgery")

  formula.bin = as.list(NULL)
  for (j in 1:length(formula)) {
    formula.bin[[j]] = as.formula(paste(sub("Pain",replacement="bin.Pain",as.character(formula[[j]]))[c(2,3)],collapse=" ~ "))
  }

  result = as.list(NULL)
  for (i in seq_along(formula)) {
    if (model=="zeroinfl.nb") {
      result[[i]] = zeroinfl(formula[[i]], data = newdata, dist="negbin")
    } else if (model=="zeroinfl.poi") {
      result[[i]] = zeroinfl(formula[[i]], data = newdata, dist="poisson")
    } else if (model=="lm") {
      result[[i]] = lm(formula[[i]], data = newdata)
    } else if (model=="glm.bin") {
      result[[i]] = glm(formula.bin[[i]], data = newdata, family = "binomial")
    } else if (model=="glm.nb") {
      result[[i]] = glm.nb(formula[[i]], data = newdata)
    } else if (model=="glm.poi") {
      result[[i]] = glm(formula[[i]], data = newdata, family = "poisson")
    } else if (model=="hurdle.nb") {
      result[[i]] = hurdle(formula[[i]], data = newdata, dist="negbin")
    } else if (model=="hurdle.poi") {
      result[[i]] = hurdle(formula[[i]], data = newdata, dist="poisson")
    }
  }
  ## Write table
  tab_model(result, digits = 3,
            dv.labels = paste("Model",1:length(formula),sep="."),
            title = paste(c(pain.type,model),collapse=" / "),
            transform="exp",
            show.zeroinf=TRUE, show.intercept=TRUE, show.se=TRUE, show.ci=FALSE, collapse.se=FALSE,
            show.p = TRUE, p.style="numeric_stars")
}

#'
#' @export
get_resultsTable = function(formula,data,
                            pain.type=c("mean","Q1st","Q2nd","Q3rd","max"),
                            model=c("zeroinfl.nb","zeroinfl.poi","lm","glm.bin","glm.nb","glm.poi","hurdle.nb","hurdle.poi")) {
  require(sjPlot)
  newdata = data
  pain.type = match.arg(pain.type,c("mean","Q1st","Q2nd","Q3rd","max"))
  # model0 = c("zeroinfl.nb","zeroinfl.poi","lm","glm.bin","glm.nb","glm.poi","hurdle.nb","hurdle.poi")
  # model = model0[(model0 %in% model)]
  colnames(newdata)[which(grepl(pattern=pain.type,colnames(newdata))==T)] = c("Pain","Pain.bfchemo","Pain.afchemo")

  formula.bin = as.formula(paste(sub("Pain",replacement="bin.Pain",as.character(formula))[c(2,3)],collapse=" ~ "))

  result = as.list(NULL)
  for (i in seq_along(model)) {
    if (model[i]=="zeroinfl.nb") {
      result[[i]] = zeroinfl(formula, data = newdata, dist="negbin")
    } else if (model[i]=="zeroinfl.poi") {
      result[[i]] = zeroinfl(formula, data = newdata, dist="poisson")
    } else if (model[i]=="lm") {
      result[[i]] = lm(formula, data = newdata)
    } else if (model[i]=="glm.bin") {
      result[[i]] = glm(formula.bin, data = newdata, family = "binomial")
    } else if (model[i]=="glm.nb") {
      result[[i]] = glm.nb(formula, data = newdata)
    } else if (model[i]=="glm.poi") {
      result[[i]] = glm(formula, data = newdata, family = "poisson")
    } else if (model[i]=="hurdle.nb") {
      result[[i]] = hurdle(formula, data = newdata, dist="negbin")
    } else if (model[i]=="hurdle.poi") {
      result[[i]] = hurdle(formula, data = newdata, dist="poisson")
    }
  }

  tab_model(result, digits = 3,
            dv.labels = model,
            title = paste("Pain type =",pain.type),
            transform="exp",
            show.zeroinf=TRUE, show.intercept=TRUE, show.se=FALSE, show.ci=FALSE, collapse.se=FALSE,
            show.p = TRUE, p.style="both")
}
