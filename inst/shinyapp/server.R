options(warn = -1)
# route messages to output in the server
if (!interactive()) sink(stderr(), type = "output")

setRzip()
animationmethod <- animationMethod()
translation.fil <- paste0(translationDir(), "/translation.bin")
load(translation.fil)
cat(paste0("preparation> Translation file loaded from: ", tools::file_path_as_absolute(translation.fil), " (", NROW(translation), " items)"), "\n")
cat("preparation> end\n")

shinyServer(function(input, output, session) {
  #####################################
  ### GLOBAL VARIABLES TO BE USED IN THE SERVER
  #####################################

  values <- reactiveValues(
    origdata = NULL, plotdata = NULL, clickdata = NULL, idscreated = NULL,
    optimizegraphs = NULL, locale = Sys.getlocale(), gwidth1 = 800, gheight1 = 600, gwidth2 = 800, gheight2 = 450, mwidth1 = 533, mheight1 = 400, mwidth2 = 711, mheight2 = 400
  )

  default_values <- list(
    colObservedLines = "default",
    colObservedPoints = "default",
    colEpidemicStart = "default",
    colEpidemicStop = "default",
    colThresholds = "default",
    colLevels = "default",
    colSeasons = "default",
    colEpidemic = "default",
    yaxis0 = TRUE,
    method = 2,
    param = list(value = 2.8, min = 0.5, max = 10, step = 0.1),
    optimparam = list(value = 2.8, min = 0.5, max = 10, step = 0.1),
    nvalues = -1,
    ntails = list(value = 1, min = 1, max = 2, step = 1),
    typethreshold = 5,
    typeintensity = 6,
    levelintensitym = list(value = 40, min = 0.5, max = 99.5, step = 0.5),
    levelintensityh = list(value = 90, min = 0.5, max = 99.5, step = 0.5),
    levelintensityv = list(value = 97.5, min = 0.5, max = 99.5, step = 0.5),
    validation = "cross",
    optimmethod = "matthews",
    paramrange = list(value = c(1, 5), min = 0.1, max = 10, step = 0.1),
    minsensitivity = list(value = 0, min = 0, max = 1, step = 0.05),
    minspecificity = list(value = 0, min = 0, max = 1, step = 0.05),
    typeaveragecurve = 2,
    typeother = 3,
    levelaveragecurve = list(value = 95.0, min = 0.5, max = 99.5, step = 0.5),
    levelother = list(value = 95.0, min = 0.5, max = 99.5, step = 0.5),
    centering = -1,
    showadvanced = TRUE,
    advanced = TRUE,
    showexperimental = TRUE,
    experimental = TRUE,
    processdata = FALSE,
    usetdistribution = FALSE,
    preepidemicthr = FALSE,
    postepidemicthr = FALSE,
    intensitythr = FALSE,
    plottiming = FALSE,
    transformation = 1,
    loesspan = list(min = 0.05, max = 1, value = 0.50, step = 0.05),
    movavgweeks = list(min = 1, max = 5, value = 3, step = 1),
    waves = 1,
    twowavesproportion = list(min = 0, max = 100, value = 15, step = 5),
    numberwaves = list(value = 0, min = 0, max = NA, step = 1),
    wavesseparation = list(value = 1, min = 0, max = NA, step = 1),
    wavesparam1 = list(value = 3, min = 0.5, max = 10, step = 0.1),
    wavesparam2 = list(value = 2, min = 0.5, max = 10, step = 0.1),
    smregressionoptimum = TRUE,
    smregressionsmoothing = list(min = 0.1, max = 5, value = 1, step = 0.1),
    transfpositive = FALSE
  )

  #####################################
  ### SERVER-SIDE FUNCTIONS
  #####################################

  trloc <- function(i.text, i.trans = FALSE) {
    txtres <- as.character(sapply(i.text, function(s) {
      o.text.en <- tail(translation[translation$original == s, "en_GB"])
      if (NROW(o.text.en) != 1) o.text.en <- s
      if (is.na(o.text.en)) o.text.en <- s
      o.text <- tail(translation[translation$original == s, input$language])
      if (NROW(o.text) != 1) o.text <- o.text.en
      if (is.na(o.text)) o.text <- o.text.en
      o.text
    }, USE.NAMES = FALSE))
    if (i.trans) txtres <- stringi::stri_trans_general(txtres, "Latin-ASCII")
    txtres
  }

  plotSeasons <- function(i.data,
                          i.pre.epidemic = TRUE,
                          i.post.epidemic = TRUE,
                          i.epidemic.thr = NA,
                          i.intensity = TRUE,
                          i.intensity.thr = NA,
                          i.range.x = NA,
                          i.range.y = NA,
                          i.tickmarks = 30,
                          i.textMain = "",
                          i.textX = "",
                          i.textY = "",
                          i.colObservedPoints = "#000000",
                          i.colSeasons = NA,
                          i.colThresholds = c("#8c6bb1", "#88419d", "#810f7c", "#4d004b", "#c0c0ff"),
                          i.yaxis.starts.at.0 = FALSE,
                          ...) {
    if (is.null(i.data)) {
      p <- NULL
    } else {
      if (any(is.na(i.colSeasons))) i.colSeasons <- colorRampPalette(RColorBrewer::brewer.pal(max(3, min(8, NCOL(i.data))), "Accent"))(NCOL(i.data))
      if (any(is.na(i.range.x)) || any(!is.numeric(i.range.x)) | length(i.range.x) != 2) i.range.x <- c(min(as.numeric(rownames(i.data)[seq_len(min(3, NROW(i.data)))])), max(as.numeric(rownames(i.data)[(max(1, NROW(i.data) - 2)):NROW(i.data)])))
      if (i.range.x[1] < 1) i.range.x[1] <- 1
      if (i.range.x[1] > 52) i.range.x[1] <- 52
      if (i.range.x[2] < 1) i.range.x[2] <- 1
      if (i.range.x[2] > 52) i.range.x[2] <- 52
      if (i.range.x[1] == i.range.x[2]) i.range.x[2] <- i.range.x[2] - 1
      if (i.range.x[2] == 0) i.range.x[2] <- 52
      # Input scheme numbering
      week.f <- i.range.x[1]
      week.l <- i.range.x[2]
      last.week <- 52
      if (week.f > week.l) {
        i.range.x.values <- data.frame(week.lab = c(week.f:last.week, seq_len(week.l)), week.no = seq_len(last.week - week.f + 1 + week.l))
      } else {
        i.range.x.values <- data.frame(week.lab = week.f:week.l, week.no = seq_len(week.l - week.f + 1))
      }

      if (NCOL(i.data) > 1) {
        epi <- memmodel(i.data,
          i.seasons = NA,
          ...
        )
        if (!is.null(epi)) {
          epidata <- epi$data
          epiindex <- as.data.frame(epi$season.indexes[, , 1])
          rownames(epiindex) <- rownames(epidata)
          colnames(epiindex) <- colnames(epidata)
          epithresholds <- epi$epidemic.thresholds
          intthresholds <- epi$intensity.thresholds
          i.data <- i.data[names(i.data) %in% names(epidata)]
        } else {
          epidata <- i.data
          epiindex <- i.data
          epiindex[!is.na(i.data)] <- 1
          epithresholds <- NA
          intthresholds <- NA
        }
      } else {
        # I need the epi object to extract the data dataframe, which includes the original data + filled missing data and
        # the timing (which would be extracted with memtiming also)
        epi <- memmodel(cbind(i.data, i.data),
          i.seasons = NA,
          ...
        )
        if (!is.null(epi)) {
          epidata <- epi$data[1]
          epiindex <- as.data.frame(epi$season.indexes[, 1, 1])
          rownames(epiindex) <- rownames(epidata)
          colnames(epiindex) <- colnames(epidata)
          epithresholds <- NA
          intthresholds <- NA
          i.data <- i.data[names(i.data) %in% names(epidata)]
        } else {
          epidata <- i.data
          epiindex <- i.data
          epiindex[!is.na(i.data)] <- 1
          epithresholds <- NA
          intthresholds <- NA
        }
      }
      rm("epi")

      # To have continuity between seasons I have to inflate original data to the global squeme. That's it: If
      # original data format is from 40 to 20, the inflated data would be 30 to 29, so that when a season ends
      # at 29, next one will start at 30 and there would be continuity between both

      data.full <- i.data
      data.full$week.lab <- rownames(data.full)
      data.full <- merge(data.full, i.range.x.values, by = "week.lab", all.y = TRUE)
      data.full <- data.full[order(data.full$week.no), ]
      row.names(data.full) <- data.full$week.lab
      data.full$week.lab <- NULL
      data.full$week.no <- NULL

      data.full.epi <- epidata
      data.full.epi$week.lab <- rownames(data.full.epi)
      data.full.epi <- merge(data.full.epi, i.range.x.values, by = "week.lab", all.y = TRUE)
      data.full.epi <- data.full.epi[order(data.full.epi$week.no), ]
      row.names(data.full.epi) <- data.full.epi$week.lab
      data.full.epi$week.lab <- NULL
      data.full.epi$week.no <- NULL

      data.full.missing <- data.full.epi
      data.full.missing[!(is.na(data.full) & !is.na(data.full.epi))] <- NA

      if (length(i.epidemic.thr) == 2) {
        epidemic <- i.epidemic.thr
      } else {
        if (NCOL(i.data) > 1) {
          epidemic <- as.numeric(epithresholds)
        } else {
          i.pre.epidemic <- FALSE
          i.post.epidemic <- FALSE
          epidemic <- NA
        }
      }

      if (length(i.intensity.thr) == 3) {
        intensity <- i.intensity.thr
      } else {
        if (NCOL(i.data) > 1) {
          intensity <- as.numeric(intthresholds)
        } else {
          i.intensity <- FALSE
          intensity <- NA
        }
      }
      labels <- c(
        names(data.full),
        paste(names(data.full), " (", trloc("graphs.missing"), ")", sep = ""),
        c(trloc("graphs.prethreshold.short"), trloc("graphs.mediumthreshold.short"), trloc("graphs.highthreshold.short"), trloc("graphs.veryhighthreshold.short"), trloc("graphs.postthreshold.short"))
      )
      haspoints <- c(rep(FALSE, NCOL(data.full)), rep(TRUE, NCOL(data.full)), FALSE, FALSE, FALSE, FALSE, FALSE)
      haslines <- c(rep(TRUE, NCOL(data.full)), rep(FALSE, NCOL(data.full)), TRUE, TRUE, TRUE, TRUE, TRUE)
      shapes <- c(rep(NA, NCOL(data.full)), rep(24, NCOL(data.full)), NA, NA, NA, NA, NA)
      colors <- c(rep(i.colSeasons, 2), i.colThresholds)
      fills <- c(rep(i.colSeasons, 2), rep(i.colObservedPoints, 5))
      sizes <- c(rep(2, NCOL(data.full)), rep(2, NCOL(data.full)), 1, 1, 1, 1, 1)
      linetypes <- c(rep("solid", NCOL(data.full)), rep("solid", NCOL(data.full)), "dashed", "dashed", "dashed", "dashed", "dashed")

      # Data to plot

      dgraf <- cbind(data.full.epi, data.full.missing,
        epit = epidemic[1],
        medt = intensity[1],
        higt = intensity[2],
        vert = intensity[3],
        post = epidemic[2]
      )
      names(dgraf) <- labels
      dgraf$week <- seq_len(NROW(dgraf))

      dgrafgg <- dgraf %>% tidyr::gather(variable, value, -week)
      dgrafgg$variable <- factor(dgrafgg$variable, levels = labels, labels = labels)

      selected.indicators <- (seq_len(2 * NCOL(data.full)))[apply(dgraf[seq_len(2 * NCOL(data.full))], 2, function(x) !all(is.na(x)))]
      if (i.pre.epidemic) selected.indicators <- c(selected.indicators, 2 * NCOL(data.full) + 1)
      if (i.post.epidemic) selected.indicators <- c(selected.indicators, 2 * NCOL(data.full) + 5)
      if (i.intensity) selected.indicators <- c(selected.indicators, 2 * NCOL(data.full) + 2:4)
      selected.indicators <- unique(selected.indicators)
      selected.indicators <- selected.indicators[order(selected.indicators)]

      labels.s <- labels[selected.indicators]
      haspoints.s <- haspoints[selected.indicators]
      haslines.s <- haslines[selected.indicators]
      dgrafgg.s <- subset(dgrafgg, variable %in% labels.s)
      shapes.s <- shapes[selected.indicators]
      colors.s <- colors[selected.indicators]
      fills.s <- fills[selected.indicators]
      sizes.s <- sizes[selected.indicators]
      linetypes.s <- linetypes[selected.indicators]

      # Calculate ticks for x
      axis.x.range.original <- range(i.range.x.values$week.no)
      axis.x.otick <- mem:::optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], i.tickmarks, seq_len(axis.x.range.original[2]), TRUE, TRUE)
      axis.x.range <- axis.x.otick$range
      axis.x.ticks <- axis.x.otick$tickmarks
      axis.x.labels <- i.range.x.values$week.lab[axis.x.otick$tickmarks]

      # Range y fix
      if (length(i.range.y) != 2) {
        if (i.yaxis.starts.at.0) {
          i.range.y <- c(0, 1.05 * max(subset(dgrafgg.s, variable != "week", select = "value"), na.rm = TRUE))
        } else {
          i.range.y <- c(0.95 * min(subset(dgrafgg.s, variable != "week", select = "value"), na.rm = TRUE), 1.05 * max(subset(dgrafgg.s, variable != "week", select = "value"), na.rm = TRUE))
        }
      }
      axis.y.range.original <- i.range.y
      axis.y.otick <- mem:::optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
      axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range)) * 0.025 * c(-1, 1)
      axis.y.ticks <- axis.y.otick$tickmarks
      axis.y.labels <- axis.y.otick$tickmarks
      # Rounding values
      dgrafgg.s$value <- round(dgrafgg.s$value, 2)
      gplot <- ggplot(dgrafgg.s) +
        geom_line(aes(x = week, y = value, group = variable, color = variable, linetype = variable), size = 0.5) +
        geom_point(aes(x = week, y = value, group = variable, color = variable, size = variable, fill = variable, shape = variable), color = "#ffffff", stroke = 0.1) +
        scale_shape_manual(values = shapes.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_color_manual(values = colors.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_fill_manual(values = fills.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_size_manual(values = sizes.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_linetype_manual(values = linetypes.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
        scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
        labs(title = i.textMain, x = i.textX, y = i.textY) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5))
      p <- list(
        plot = gplot, labels = labels.s, haspoints = haspoints.s, haslines = haslines.s,
        weeklabels = i.range.x.values$week.lab, gdata = dgrafgg.s
      )
    }
    p
  }

  plotSeries <- function(i.data,
                         i.plot.timing = TRUE,
                         i.pre.epidemic = TRUE,
                         i.post.epidemic = TRUE,
                         i.epidemic.thr = NA,
                         i.intensity = TRUE,
                         i.intensity.thr = NA,
                         i.range.x = NA,
                         i.range.y = NA,
                         i.tickmarks = 30,
                         i.replace.x.cr = FALSE,
                         i.textMain = "",
                         i.textX = "",
                         i.textY = "",
                         i.colObservedLines = "#808080",
                         i.colThresholds = c("#8c6bb1", "#88419d", "#810f7c", "#4d004b", "#c0c0ff"),
                         i.colObservedPoints = "#000000",
                         i.colEpidemic = c("#00C000", "#800080", "#FFB401"),
                         i.yaxis.starts.at.0 = FALSE,
                         ...) {
    if (is.null(i.data)) {
      p <- NULL
    } else {
      # Range x fix
      i.cutoff.original <- min(as.numeric(rownames(i.data)[seq_len(min(3, NROW(i.data)))]))
      if (i.cutoff.original < 1) i.cutoff.original <- 1
      if (i.cutoff.original > 52) i.cutoff.original <- 52
      if (any(is.na(i.range.x)) | !is.numeric(i.range.x) | length(i.range.x) != 2) i.range.x <- c(min(as.numeric(rownames(i.data)[seq_len(min(3, NROW(i.data)))])), max(as.numeric(rownames(i.data)[(max(1, NROW(i.data) - 2)):NROW(i.data)])))
      if (i.range.x[1] < 1) i.range.x[1] <- 1
      if (i.range.x[1] > 52) i.range.x[1] <- 52
      if (i.range.x[2] < 1) i.range.x[2] <- 1
      if (i.range.x[2] > 52) i.range.x[2] <- 52
      if (i.range.x[1] == i.range.x[2]) i.range.x[2] <- i.range.x[2] - 1
      if (i.range.x[2] == 0) i.range.x[2] <- 52
      week.f <- i.range.x[1]
      week.l <- i.range.x[2]
      last.week <- 52
      if (week.f > week.l) {
        i.range.x.values <- data.frame(week.lab = c(week.f:last.week, seq_len(week.l)), week.no = seq_len(last.week - week.f + 1 + week.l))
      } else {
        i.range.x.values <- data.frame(week.lab = week.f:week.l, week.no = seq_len(week.l - week.f + 1))
      }

      if (NCOL(i.data) > 1) {
        epi <- memmodel(i.data,
          i.seasons = NA,
          ...
        )
        if (!is.null(epi)) {
          epidata <- epi$data
          epiindex <- as.data.frame(epi$season.indexes[, , 1])
          rownames(epiindex) <- rownames(epidata)
          colnames(epiindex) <- colnames(epidata)
          epithresholds <- epi$epidemic.thresholds
          intthresholds <- epi$intensity.thresholds
          i.data <- i.data[names(i.data) %in% names(epidata)]
        } else {
          epidata <- i.data
          epiindex <- i.data
          epiindex[!is.na(i.data)] <- 1
          epithresholds <- NA
          intthresholds <- NA
        }
      } else {
        # I need the epi object to extract the data dataframe, which includes the original data + filled missing data and
        # the timing (which would be extracted with memtiming also)
        epi <- memmodel(cbind(i.data, i.data),
          i.seasons = NA,
          ...
        )
        if (!is.null(epi)) {
          epidata <- epi$data[1]
          epiindex <- as.data.frame(epi$season.indexes[, 1, 1])
          rownames(epiindex) <- rownames(epidata)
          colnames(epiindex) <- colnames(epidata)
          epithresholds <- NA
          intthresholds <- NA
          i.data <- i.data[names(i.data) %in% names(epidata)]
        } else {
          epidata <- i.data
          epiindex <- i.data
          epiindex[!is.na(i.data)] <- 1
          epithresholds <- NA
          intthresholds <- NA
        }
      }
      rm("epi")

      # To have continuity between seasons I have to inflate original data to the global squeme. That's it: If
      # original data format is from 40 to 20, the inflated data would be 30 to 29, so that when a season ends
      # at 29, next one will start at 30 and there would be continuity between both

      data.full <- i.data
      data.full$week.lab <- rownames(data.full)
      data.full <- merge(data.full, i.range.x.values, by = "week.lab", all.y = TRUE)
      data.full <- data.full[order(data.full$week.no), ]
      row.names(data.full) <- data.full$week.lab
      data.full$week.lab <- NULL
      data.full$week.no <- NULL

      data.full.epi <- epidata
      data.full.epi$week.lab <- rownames(data.full.epi)
      data.full.epi <- merge(data.full.epi, i.range.x.values, by = "week.lab", all.y = TRUE)
      data.full.epi <- data.full.epi[order(data.full.epi$week.no), ]
      row.names(data.full.epi) <- data.full.epi$week.lab
      data.full.epi$week.lab <- NULL
      data.full.epi$week.no <- NULL

      data.full.index <- epiindex
      data.full.index[is.na(epidata)] <- NA
      data.full.index$week.lab <- rownames(data.full.index)
      data.full.index <- merge(data.full.index, i.range.x.values, by = "week.lab", all.y = TRUE)
      data.full.index <- data.full.index[order(data.full.index$week.no), ]
      row.names(data.full.index) <- data.full.index$week.lab
      data.full.index$week.lab <- NULL
      data.full.index$week.no <- NULL

      # Data to plot
      data.orig <- transformdata.back(data.full, i.name = "rates", i.cutoff.original = i.cutoff.original, i.range.x.final = i.range.x, i.fun = sum)$data
      data.y <- as.numeric(data.orig[, "rates"])
      # Data to plot, filling in missing with data imputed by mem (using loess)
      data.fixed <- transformdata.back(data.full.epi, i.name = "rates", i.cutoff.original = i.cutoff.original, i.range.x.final = i.range.x, i.fun = sum)$data
      data.y.fixed <- as.numeric(data.fixed[, "rates"])
      # Data that have been imputed, to mark them as a circle with a cross
      data.missing <- data.fixed
      data.missing[!(is.na(data.orig) & !is.na(data.fixed))] <- NA
      data.y.missing <- as.numeric(data.missing[, "rates"])
      # Indexes for pre, epi and post epidemic
      data.indexes <- transformdata.back(data.full.index, i.name = "rates", i.cutoff.original = i.cutoff.original, i.range.x.final = i.range.x, i.fun = function(x, ...) {
        if (all(is.na(x))) {
          return(NA)
        } else if (any(x == 2, ...)) {
          return(2)
        } else if (any(x == 1, ...)) {
          return(1)
        } else {
          return(3)
        }
      })$data
      data.y.indexes <- as.numeric(data.indexes[, names(data.indexes) == "rates"])

      if (length(i.epidemic.thr) == 2) {
        epidemic <- i.epidemic.thr
      } else {
        if (NCOL(i.data) > 1) {
          epidemic <- as.numeric(epithresholds)
        } else {
          i.pre.epidemic <- FALSE
          i.post.epidemic <- FALSE
          epidemic <- NA
        }
      }

      if (length(i.intensity.thr) == 3) {
        intensity <- i.intensity.thr
      } else {
        if (NCOL(i.data) > 1) {
          intensity <- as.numeric(intthresholds)
        } else {
          i.intensity <- FALSE
          intensity <- NA
        }
      }

      labels <- c(trloc("graphs.weeklydata"), trloc("graphs.preepidemic"), trloc("graphs.preepidemicmiss"), trloc("graphs.epidemic"), trloc("graphs.epidemicmiss"), trloc("graphs.postepidemic"), trloc("graphs.postepidemicmiss"), trloc("graphs.prethreshold.short"), trloc("graphs.mediumthreshold.short"), trloc("graphs.highthreshold.short"), trloc("graphs.veryhighthreshold.short"), trloc("graphs.postthreshold.short"))
      haspoints <- c(ifelse(i.plot.timing, FALSE, TRUE), TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
      haslines <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
      shapes <- c(21, 21, 24, 21, 24, 21, 24, NA, NA, NA, NA, NA)
      colors <- c(rep(i.colObservedLines, 7), i.colThresholds)
      fills <- c(i.colObservedPoints, rep(i.colEpidemic, each = 2), rep(i.colObservedPoints, 5))
      sizes <- c(2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1)
      linetypes <- c("solid", "blank", "blank", "blank", "blank", "blank", "blank", "dashed", "dashed", "dashed", "dashed", "dashed")

      dgraf <- data.frame(
        rates = data.y.fixed,
        prer = data.y,
        prem = data.y.missing,
        epir = data.y,
        epim = data.y.missing,
        posr = data.y,
        posm = data.y.missing,
        epit = epidemic[1],
        medt = intensity[1],
        higt = intensity[2],
        vert = intensity[3],
        post = epidemic[2]
      )
      dgraf$prer[data.y.indexes != 1] <- NA
      dgraf$prem[data.y.indexes != 1] <- NA
      dgraf$epir[data.y.indexes != 2] <- NA
      dgraf$epim[data.y.indexes != 2] <- NA
      dgraf$posr[data.y.indexes != 3] <- NA
      dgraf$posm[data.y.indexes != 3] <- NA
      names(dgraf) <- labels
      dgraf$week <- seq_len(NROW(dgraf))

      dgrafgg <- dgraf %>% tidyr::gather(variable, value, -week)
      dgrafgg$variable <- factor(dgrafgg$variable, levels = labels, labels = labels)

      selected.indicators <- 1
      if (i.plot.timing) {
        selected.indicators <- c(selected.indicators, c(2, 4, 6))
        if (!all(is.na(dgraf[, 3]))) selected.indicators <- c(selected.indicators, 3)
        if (!all(is.na(dgraf[, 5]))) selected.indicators <- c(selected.indicators, 5)
        if (!all(is.na(dgraf[, 7]))) selected.indicators <- c(selected.indicators, 7)
      }
      if (i.pre.epidemic) selected.indicators <- c(selected.indicators, 8)
      if (i.post.epidemic) selected.indicators <- c(selected.indicators, 12)
      if (i.intensity) selected.indicators <- c(selected.indicators, 9:11)
      selected.indicators <- unique(selected.indicators)
      selected.indicators <- selected.indicators[order(selected.indicators)]

      labels.s <- labels[selected.indicators]
      haspoints.s <- haspoints[selected.indicators]
      haslines.s <- haslines[selected.indicators]
      dgrafgg.s <- subset(dgrafgg, variable %in% labels.s)
      shapes.s <- shapes[selected.indicators]
      colors.s <- colors[selected.indicators]
      fills.s <- fills[selected.indicators]
      sizes.s <- sizes[selected.indicators]
      linetypes.s <- linetypes[selected.indicators]
      # Calculate ticks for x
      data.x <- seq_len(NROW(data.orig))
      axis.x.range <- range(data.x)
      temp1 <- range(i.range.x.values$week.no)
      temp2 <- mem:::optimal.tickmarks(temp1[1], temp1[2], floor(i.tickmarks / NCOL(i.data)), seq_len(temp1[2]), TRUE, FALSE)
      temp3 <- floor(mean(i.range.x.values$week.no))
      # Ticks for the weeks
      axis.x.ticks.1 <- data.x[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
      # Ticks for the seasons
      axis.x.ticks.2 <- data.x[data.orig$week %in% i.range.x.values$week.lab[temp3]]
      # Labels for the week-ticks
      axis.x.labels1 <- data.orig$week[data.orig$week %in% i.range.x.values$week.lab[temp2$tickmarks]]
      # Labels for the season-ticks
      axis.x.labels2 <- data.orig$season[data.orig$week %in% i.range.x.values$week.lab[temp3]]
      # I join both type of ticks, maybe they are in the same position
      axis.x.ticks <- sort(unique(c(axis.x.ticks.1, axis.x.ticks.2)))
      # Part of the final label of the week
      temp4 <- rep("", length(axis.x.ticks))
      temp4[axis.x.ticks %in% axis.x.ticks.1] <- axis.x.labels1
      # Part of the final label of the year
      temp5 <- rep("", length(axis.x.ticks))
      temp5[axis.x.ticks %in% axis.x.ticks.2] <- axis.x.labels2
      # And paste both parts
      axis.x.labels <- paste(temp4, temp5, sep = "\n")
      if (i.replace.x.cr) axis.x.labels <- gsub("/", "\n", axis.x.labels)
      # This is not to print a tickmark when there is only a season label, tickmarks are only for weeks
      axis.x.tickmarks <- rep(NA, length(axis.x.ticks))
      axis.x.tickmarks[axis.x.ticks %in% axis.x.ticks.1] <- "black"
      rm("temp1", "temp2", "temp3", "temp4", "temp5")
      # Range y fix
      if (length(i.range.y) != 2) {
        if (i.yaxis.starts.at.0) {
          i.range.y <- c(0, 1.05 * max(subset(dgrafgg.s, variable != "week", select = "value"), na.rm = TRUE))
        } else {
          i.range.y <- c(0.95 * min(subset(dgrafgg.s, variable != "week", select = "value"), na.rm = TRUE), 1.05 * max(subset(dgrafgg.s, variable != "week", select = "value"), na.rm = TRUE))
        }
      }
      axis.y.range.original <- i.range.y
      axis.y.otick <- mem:::optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
      axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range)) * 0.025 * c(-1, 1)
      axis.y.ticks <- axis.y.otick$tickmarks
      axis.y.labels <- axis.y.otick$tickmarks
      dgrafgg.s$value <- round(dgrafgg.s$value, 2)

      gplot <- ggplot(dgrafgg.s) +
        geom_line(aes(x = week, y = value, group = variable, color = variable, linetype = variable), size = 0.5) +
        geom_point(aes(x = week, y = value, group = variable, color = variable, size = variable, fill = variable, shape = variable), color = "#ffffff", stroke = 0.1) +
        scale_shape_manual(values = shapes.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_color_manual(values = colors.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_fill_manual(values = fills.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_size_manual(values = sizes.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_linetype_manual(values = linetypes.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
        scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
        labs(title = i.textMain, x = i.textX, y = i.textY) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5), axis.ticks.x = element_line(color = axis.x.tickmarks))
      p <- list(
        plot = gplot, labels = labels.s, haspoints = haspoints.s, haslines = haslines.s,
        weeklabels = paste(data.orig$week, paste0("<br />", trloc("selections.surveillance.season.label"), ": "), data.orig$season, sep = ""), gdata = dgrafgg.s
      )
    }
    p
  }

  plotSurveillance <- function(i.data,
                               i.week.report = NA,
                               i.range.x = NA,
                               i.range.y = NA,
                               i.pre.epidemic = TRUE,
                               i.post.epidemic = TRUE,
                               i.epidemic = TRUE,
                               i.start = TRUE,
                               i.end = TRUE,
                               i.epidemic.thr = NA,
                               i.intensity = TRUE,
                               i.intensity.thr = NA,
                               i.mean.length = 10,
                               i.force.length = FALSE,
                               i.force.equal = FALSE,
                               i.force.start = NA,
                               i.force.week.53 = FALSE,
                               i.textMain = "",
                               i.textX = "",
                               i.textY = "",
                               i.colObservedLines = "#808080",
                               i.colObservedPoints = "#000000",
                               i.colEpidemicStart = "#FF0000",
                               i.colEpidemicStop = "#40FF40",
                               i.colThresholds = c("#8c6bb1", "#88419d", "#810f7c", "#4d004b", "#c0c0ff"),
                               i.yaxis.starts.at.0 = FALSE) {
    # check parameters
    if (is.null(i.data)) {
      p <- NULL
    } else if (is.null(dim(i.data))) {
      p <- NULL
    } else if (!(ncol(i.data) == 1)) {
      p <- NULL
    } else {
      if (i.force.week.53) last.week <- 53 else last.week <- 52

      if (!is.numeric(i.range.x) | length(i.range.x) != 2) i.range.x <- c(max(1, as.numeric(rownames(i.data)[1])), min(52, as.numeric(rownames(i.data)[NROW(i.data)])))
      week.f <- i.range.x[1]
      week.l <- i.range.x[2]
      if (week.f < 1) week.f <- 1
      if (week.f > 52) week.f <- 52
      if (week.l < 1) week.l <- 1
      if (week.l > 52) week.l <- 52
      if (week.f == week.l) week.l <- week.l - 1
      last.week <- 52
      if (week.f > week.l) {
        i.range.x.values <- data.frame(week.lab = c(week.f:last.week, seq_len(week.l)), week.no = seq_len(last.week - week.f + 1 + week.l))
      } else {
        i.range.x.values <- data.frame(week.lab = week.f:week.l, week.no = seq_len(week.l - week.f + 1))
      }

      if (length(i.epidemic.thr) != 2) {
        i.pre.epidemic <- FALSE
        i.post.epidemic <- FALSE
      }

      if (length(i.intensity.thr) != 3) i.intensity <- FALSE

      if (!is.numeric(i.epidemic.thr) | length(i.epidemic.thr) == 1) i.epidemic.thr <- rep(NA, 2)
      if (!is.numeric(i.intensity.thr) | length(i.intensity.thr) == 1) i.intensity.thr <- rep(NA, 3)

      # Esquema de las semanas

      esquema.temporadas.1 <- last.week
      if (i.range.x[1] == i.range.x[2]) i.range.x[2] <- i.range.x[1] - 1
      if (i.range.x[1] < i.range.x[2]) {
        esquema.temporadas.2 <- max(1, i.range.x[1])
        esquema.temporadas.3 <- min(esquema.temporadas.1, i.range.x[2])
        esquema.temporadas.4 <- c(esquema.temporadas.2:esquema.temporadas.3)
      } else {
        esquema.temporadas.2 <- min(esquema.temporadas.1, i.range.x[1])
        esquema.temporadas.3 <- max(1, i.range.x[2])
        esquema.temporadas.4 <- c(esquema.temporadas.2:esquema.temporadas.1, seq_len(esquema.temporadas.3))
      }
      semanas <- length(esquema.temporadas.4)
      esquema.semanas <- data.frame(numero.semana = seq_len(semanas), nombre.semana = esquema.temporadas.4)

      # Acomodamos i.data al esquema
      current.season <- i.data
      names(current.season) <- "rates"
      current.season$nombre.semana <- rownames(i.data)
      rownames(current.season) <- NULL
      current.season <- merge(esquema.semanas, current.season, by = "nombre.semana", all.x = TRUE)
      current.season <- current.season[order(current.season$numero.semana), ]
      rownames(current.season) <- NULL

      # limitamos a la semana del informe (i.week.report)
      if (!is.na(i.week.report) & any(i.week.report == as.numeric(esquema.semanas$nombre.semana))) {
        semana.report <- ((seq_len(semanas))[i.week.report == as.numeric(esquema.semanas$nombre.semana)])[1]
        if (!is.na(semana.report) & semana.report < semanas) current.season$rates[(semana.report + 1):semanas] <- NA
      } else {
        if (all(is.na(current.season$rates))) semana.report <- semanas else semana.report <- max((seq_len(semanas))[!is.na(current.season$rates)], na.rm = TRUE)
      }

      # Preparacion de datos necesarios
      umbral.pre <- as.numeric(i.epidemic.thr[1])
      if (i.force.equal) umbral.pos <- as.numeric(i.epidemic.thr[1]) else umbral.pos <- as.numeric(i.epidemic.thr[2])
      duracion.media <- i.mean.length

      # Si el inicio forzado de la epidemia es posterior a la semana del informe, quitamos
      if (!is.na(i.force.start)) semana.inicio.forzado <- ((seq_len(semanas))[i.force.start == as.numeric(esquema.semanas$nombre.semana)])[1] else semana.inicio.forzado <- NA
      if (any(current.season$rates > umbral.pre, na.rm = TRUE)) semana.inicio.real <- min((seq_len(semanas))[current.season$rates > umbral.pre], na.rm = TRUE) else semana.inicio.real <- NA
      if (!is.na(semana.inicio.forzado)) {
        if (semana.inicio.forzado > semana.report) semana.inicio.forzado <- NA
      }
      if (!is.na(semana.inicio.forzado) & !is.na(semana.inicio.real)) {
        if (semana.inicio.forzado == semana.inicio.real) semana.inicio.forzado <- NA
      }
      if (!is.na(semana.inicio.forzado)) {
        semana.inicio <- semana.inicio.forzado
      } else {
        semana.inicio <- semana.inicio.real
      }

      week.peak <- which.max(current.season$rates)

      if (!is.na(semana.inicio)) {
        if (i.force.length) {
          semana.fin <- semana.inicio + i.mean.length
          if (semana.fin > semanas) semana.fin <- NA
        } else {
          punto.de.busqueda <- max(semana.inicio, semana.inicio.real, week.peak, na.rm = TRUE)
          semana.fin.1 <- (seq_len(semanas))[current.season$rates < umbral.pos & punto.de.busqueda < (seq_len(semanas))]
          if (any(semana.fin.1, na.rm = TRUE)) semana.fin <- min(semana.fin.1, na.rm = TRUE) else semana.fin <- NA
        }
      } else {
        semana.fin <- NA
      }
      if (!i.epidemic) {
        semana.inicio <- NA
        semana.fin <- NA
      }
      limites.niveles <- as.vector(i.intensity.thr)
      limites.niveles[limites.niveles < 0] <- 0

      # Datos para el grafico
      if (is.na(semana.inicio)) {
        # No iniciada
        pre.umbrales.1 <- rep(umbral.pre, semana.report + 1)
        pre.umbrales.2 <- rep(NA, semanas)
        post.umbrales.1 <- rep(NA, semana.report + 1)
        post.umbrales.2 <- rep(NA, semanas)
        intensidades.1 <- array(dim = c(semanas, 3))
        intensidades.2 <- array(dim = c(semanas, 3))
      } else {
        if (is.na(semana.fin)) {
          # Iniciada y no finalizada
          pre.umbrales.1 <- rep(umbral.pre, semana.inicio - 1)
          pre.umbrales.2 <- rep(NA, max(duracion.media, semana.report - semana.inicio + 1))
          post.umbrales.1 <- rep(NA, semana.inicio - 1)
          post.umbrales.2 <- rep(NA, max(duracion.media, semana.report - semana.inicio + 1))
          if (i.intensity) {
            intensidades.1 <- array(dim = c(semana.inicio - 1, 3))
            intensidades.2 <- matrix(rep(limites.niveles, max(duracion.media, semana.report - semana.inicio + 1)), ncol = 3, byrow = TRUE)
          } else {
            intensidades.1 <- array(dim = c(semana.inicio - 1, 3))
            intensidades.2 <- array(dim = c(max(duracion.media, semana.report - semana.inicio + 1), 3))
          }
        } else {
          # Iniciada y finalizada
          pre.umbrales.1 <- rep(umbral.pre, semana.inicio - 1)
          pre.umbrales.2 <- rep(NA, semana.fin - semana.inicio)
          post.umbrales.1 <- rep(NA, semana.inicio - 1)
          post.umbrales.2 <- rep(NA, semana.fin - semana.inicio)
          if (i.intensity) {
            intensidades.1 <- array(dim = c(semana.inicio - 1, 3))
            intensidades.2 <- matrix(rep(limites.niveles, semana.fin - semana.inicio), ncol = 3, byrow = TRUE)
          } else {
            intensidades.1 <- array(dim = c(semana.inicio - 1, 3))
            intensidades.2 <- array(dim = c(semana.fin - semana.inicio, 3))
          }
        }
      }
      if (i.post.epidemic) {
        pre.umbrales.3 <- rep(NA, semanas)
        post.umbrales.3 <- rep(umbral.pos, semanas)
      } else {
        pre.umbrales.3 <- rep(NA, semanas)
        post.umbrales.3 <- rep(NA, semanas)
      }
      pre.umbrales <- c(pre.umbrales.1, pre.umbrales.2, pre.umbrales.3)[seq_len(semanas)]
      post.umbrales <- c(post.umbrales.1, post.umbrales.2, post.umbrales.3)[seq_len(semanas)]
      intensidades.3 <- array(dim = c(semanas, 3))
      intensidades <- rbind(intensidades.1, intensidades.2, intensidades.3)[seq_len(semanas), ]
      labels <- c(names(i.data), trloc("graphs.prethreshold.short"), trloc("graphs.mediumthreshold.short"), trloc("graphs.highthreshold.short"), trloc("graphs.veryhighthreshold.short"), trloc("graphs.postthreshold.short"), trloc("graphs.start"), trloc("graphs.end"))
      haspoints <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
      haslines <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
      shapes <- c(21, NA, NA, NA, NA, NA, 21, 21)
      colors <- c(i.colObservedLines, i.colThresholds, rep(i.colObservedLines, 2))
      fills <- c(rep(i.colObservedPoints, 6), i.colEpidemicStart, i.colEpidemicStop)
      sizes <- c(3, 1, 1, 1, 1, 1, 4, 4)
      linetypes <- c("solid", "dashed", "dashed", "dashed", "dashed", "dashed", "blank", "blank")

      dgraf <- as.data.frame(cbind(current.season$rates, pre.umbrales, intensidades, post.umbrales))
      dgraf$start <- NA
      dgraf$end <- NA
      if (!is.na(semana.inicio)) dgraf$start[semana.inicio] <- current.season$rates[semana.inicio]
      if (!is.na(semana.fin)) dgraf$end[semana.fin] <- current.season$rates[semana.fin]
      names(dgraf) <- labels
      dgraf$week <- seq_len(semanas)

      dgrafgg <- dgraf %>% tidyr::gather(variable, value, -week)
      dgrafgg$variable <- factor(dgrafgg$variable, levels = labels, labels = labels)

      selected.indicators <- 1
      if (i.pre.epidemic) selected.indicators <- c(selected.indicators, 2)
      if (i.post.epidemic) selected.indicators <- c(selected.indicators, 6)
      if (i.intensity) selected.indicators <- c(selected.indicators, 3:5)
      if (i.start) selected.indicators <- c(selected.indicators, 7)
      if (i.end) selected.indicators <- c(selected.indicators, 8)
      selected.indicators <- unique(selected.indicators)
      selected.indicators <- selected.indicators[order(selected.indicators)]

      labels.s <- labels[selected.indicators]
      haspoints.s <- haspoints[selected.indicators]
      haslines.s <- haslines[selected.indicators]
      dgrafgg.s <- subset(dgrafgg, variable %in% labels.s)
      shapes.s <- shapes[selected.indicators]
      colors.s <- colors[selected.indicators]
      fills.s <- fills[selected.indicators]
      sizes.s <- sizes[selected.indicators]
      linetypes.s <- linetypes[selected.indicators]

      # Axis format for all the graphs
      # Calculate values if we want to place 20 tickmarks in the graph in the x-axis.

      axis.x.range.original <- c(1, semanas)
      axis.x.otick <- mem:::optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 20, seq_len(axis.x.range.original[2]), TRUE, TRUE)
      axis.x.range <- axis.x.otick$range
      axis.x.values <- as.numeric(current.season$numero.semana)
      axis.x.ticks <- axis.x.otick$tickmarks
      axis.x.labels <- (current.season$nombre.semana)[axis.x.otick$tickmarks]
      # Same, for 10 tickmarks in the y-axis
      # Range y fix
      if (length(i.range.y) != 2) {
        if (i.yaxis.starts.at.0) {
          i.range.y <- c(0, 1.05 * max(subset(dgrafgg.s, variable != "week", select = "value"), na.rm = TRUE))
        } else {
          i.range.y <- c(0.95 * min(subset(dgrafgg.s, variable != "week", select = "value"), na.rm = TRUE), 1.05 * max(subset(dgrafgg.s, variable != "week", select = "value"), na.rm = TRUE))
        }
      }
      axis.y.range.original <- i.range.y
      axis.y.otick <- mem:::optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
      axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range)) * 0.025 * c(-1, 1)
      axis.y.ticks <- axis.y.otick$tickmarks
      axis.y.labels <- axis.y.otick$tickmarks
      dgrafgg.s$value <- round(dgrafgg.s$value, 2)
      gplot <- ggplot(dgrafgg.s) +
        geom_line(aes(x = week, y = value, group = variable, color = variable, linetype = variable), size = 1.2) +
        geom_point(aes(x = week, y = value, group = variable, color = variable, size = variable, fill = variable, shape = variable), color = "#ffffff", stroke = 0.1) +
        scale_shape_manual(values = shapes.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_color_manual(values = colors.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_fill_manual(values = fills.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_size_manual(values = sizes.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_linetype_manual(values = linetypes.s, name = trloc("graphs.legend"), labels = labels.s) +
        scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
        scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
        labs(title = i.textMain, x = i.textX, y = i.textY) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5))
      p <- list(
        plot = gplot, labels = labels.s, haspoints = haspoints.s, haslines = haslines.s,
        weeklabels = current.season$nombre.semana, gdata = dgrafgg.s
      )
    }
    p
  }

  plotGeneric <- function(i.data,
                          i.range.y,
                          i.range.y.labels = NA,
                          i.shapes,
                          i.colors,
                          i.fills,
                          i.sizes,
                          i.linetypes,
                          i.linesize,
                          i.replace.x.cr = FALSE,
                          i.textMain = "",
                          i.textX = "",
                          i.textY = "",
                          i.yaxis.starts.at.0 = FALSE) {
    if (is.null(i.data)) {
      p <- NULL
    } else {
      dgraf <- i.data
      labels <- names(dgraf)
      dgraf$num <- seq_len(NROW(dgraf))

      dgrafgg <- dgraf %>% tidyr::gather(variable, value, -num)
      dgrafgg$variable <- factor(dgrafgg$variable, levels = labels, labels = labels)

      # Calculate ticks for x
      axis.x.range <- c(1, NROW(dgraf))
      axis.x.ticks <- seq_len(NROW(dgraf))
      axis.x.labels <- rownames(dgraf)
      if (i.replace.x.cr) axis.x.labels <- gsub("/", "\n", axis.x.labels)
      # Range y fix
      if (length(i.range.y.labels) < 2) {
        if (length(i.range.y) != 2) {
          if (i.yaxis.starts.at.0) {
            i.range.y <- c(0, 1.05 * max(dgrafgg$value, na.rm = TRUE))
          } else {
            i.range.y <- c(0.95 * min(dgrafgg$value, na.rm = TRUE), 1.05 * max(dgrafgg$value, na.rm = TRUE))
          }
        }
        axis.y.range.original <- i.range.y
        axis.y.otick <- mem:::optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
        axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range)) * 0.025 * c(-1, 1)
        axis.y.ticks <- axis.y.otick$tickmarks
        axis.y.labels <- axis.y.otick$tickmarks
      } else {
        axis.y.range.original <- c(1, length(i.range.y.labels))
        axis.y.otick <- mem:::optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10,
          i.valid.ticks = seq_len(length(i.range.y.labels)), i.include.min = TRUE, i.include.max = TRUE
        )
        axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range)) * 0.025 * c(-1, 1)
        axis.y.ticks <- axis.y.otick$tickmarks
        axis.y.labels <- i.range.y.labels[axis.y.otick$tickmarks]
      }
      dgrafgg$value <- round(dgrafgg$value, 2)
      gplot <- ggplot(dgrafgg) +
        geom_line(aes(x = num, y = value, group = variable, color = variable, linetype = variable), size = i.linesize) +
        geom_point(aes(x = num, y = value, group = variable, color = variable, size = variable, fill = variable, shape = variable), color = "#ffffff", stroke = 0.1) +
        scale_shape_manual(values = i.shapes, name = trloc("graphs.legend"), labels = labels) +
        scale_color_manual(values = i.colors, name = trloc("graphs.legend"), labels = labels) +
        scale_fill_manual(values = i.fills, name = trloc("graphs.legend"), labels = labels) +
        scale_size_manual(values = i.sizes, name = trloc("graphs.legend"), labels = labels) +
        scale_linetype_manual(values = i.linetypes, name = trloc("graphs.legend"), labels = labels) +
        scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
        scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
        labs(title = i.textMain, x = i.textX, y = i.textY) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5))
      p <- list(plot = gplot, gdata = dgrafgg)
    }
    p
  }

  plotMAP <- function(i.data,
                      i.textMain = "",
                      i.textX = "",
                      i.textY = "",
                      i.method = 2,
                      i.param = 2.8,
                      i.colObservedLines = "#808080",
                      i.colObservedPoints = "#000000",
                      i.colOptimum = "#FF0000",
                      i.colLine = "#FFB401") {
    if (is.null(i.data)) {
      p <- NULL
    } else {
      timdata <- memtiming(i.data, i.method = i.method, i.param = i.param)
      dgrafgg <- as.data.frame(timdata$map.curve[, c(1, 2)])
      names(dgrafgg) <- c("weeks", "map")
      # Calculate ticks for x
      axis.x.range.original <- range(dgrafgg$weeks)
      axis.x.otick <- mem:::optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 10, TRUE, TRUE)
      axis.x.range <- axis.x.otick$range
      axis.x.ticks <- axis.x.otick$tickmarks
      axis.x.labels <- axis.x.otick$tickmarks
      # Range y fix
      axis.y.range.original <- c(0, 100)
      axis.y.otick <- mem:::optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
      axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range)) * 0.025 * c(-1, 1)
      axis.y.ticks <- axis.y.otick$tickmarks
      axis.y.labels <- axis.y.otick$tickmarks
      x.opt <- timdata$optimum.map[1]
      y.opt <- timdata$optimum.map[2]
      dgrafgg$map <- round(dgrafgg$map, 2)
      gplot <- ggplot(dgrafgg) +
        geom_line(aes(x = weeks, y = map), color = i.colObservedLines, linetype = 1, size = 1) +
        geom_point(aes(x = weeks, y = map), color = i.colObservedPoints, size = 3, shape = 21, fill = i.colObservedPoints, stroke = 0.1) +
        geom_segment(aes(x = x.opt, y = y.opt, xend = x.opt, yend = dgrafgg[1, 2]), col = i.colLine, lwd = 1) +
        geom_segment(aes(x = x.opt, y = y.opt, xend = dgrafgg[1, 1], yend = y.opt), col = i.colLine, lwd = 1) +
        geom_point(aes(x = x.opt, y = y.opt), color = i.colOptimum, size = 3, shape = 21, fill = i.colOptimum) +
        scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
        scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
        labs(title = i.textMain, x = i.textX, y = i.textY) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5))
      p <- list(plot = gplot, gdata = dgrafgg)
    }
    p
  }

  plotSlope <- function(i.data,
                        i.textMain = "",
                        i.textX = "",
                        i.textY = "",
                        i.method = 2,
                        i.param = 2.8,
                        i.colObservedLines = "#808080",
                        i.colObservedPoints = "#000000",
                        i.colOptimum = "#FF0000",
                        i.colLine1 = "#800080",
                        i.colLine2 = "#FFB401") {
    if (is.null(i.data)) {
      p <- NULL
    } else {
      if (i.method == 1) {
        timdata <- memtiming(i.data, i.method = i.method, i.param = i.param)
        dgrafgg <- timdata$slope.curve
        # Calculate ticks for x
        axis.x.range.original <- range(dgrafgg$weeks)
        axis.x.otick <- mem:::optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 15, i.valid.ticks = seq_len(5), i.include.min = TRUE, i.include.max = TRUE)
        axis.x.range <- axis.x.otick$range
        axis.x.ticks <- axis.x.otick$tickmarks
        axis.x.labels <- axis.x.otick$tickmarks
        # Range y fix
        axis.y.range.original <- range(dgrafgg$slope)
        axis.y.otick <- mem:::optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
        axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range)) * 0.025 * c(-1, 1)
        axis.y.ticks <- axis.y.otick$tickmarks
        axis.y.labels <- axis.y.otick$tickmarks
        x.opt <- timdata$optimum.map[1]
        y.opt <- dgrafgg$slope[dgrafgg$weeks == x.opt]
        b <- (dgrafgg$slope[NROW(dgrafgg)] - dgrafgg$slope[1]) / (dgrafgg$weeks[NROW(dgrafgg)] - dgrafgg$weeks[1])
        a1 <- dgrafgg$slope[x.opt] - b * x.opt
        a2 <- dgrafgg$slope[1] - b * 1
        a3 <- dgrafgg$slope[x.opt] + b * x.opt
        dgrafgg$slope <- round(dgrafgg$slope, 2)
        gplot <- ggplot(dgrafgg) +
          geom_line(aes(x = weeks, y = slope), color = i.colObservedLines, linetype = 1, size = 1) +
          geom_point(aes(x = weeks, y = slope), color = i.colObservedPoints, size = 3, shape = 21, fill = i.colObservedPoints, stroke = 0.1) +
          geom_abline(slope = b, intercept = a1, col = i.colLine1, lwd = 1.5, linetype = 2) +
          geom_abline(slope = b, intercept = a2, col = i.colLine2, lwd = 1) +
          geom_abline(slope = -b, intercept = a3, col = i.colLine2, lwd = 1) +
          geom_point(aes(x = x.opt, y = y.opt), color = i.colOptimum, size = 4, shape = 21, fill = i.colOptimum) +
          scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
          scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
          labs(title = i.textMain, x = i.textX, y = i.textY) +
          theme_light() +
          theme(plot.title = element_text(hjust = 0.5))
        p <- list(plot = gplot, gdata = dgrafgg)
      } else if (i.method == 2) {
        timdata <- memtiming(i.data, i.method = i.method, i.param = i.param)
        dgrafgg <- timdata$slope.curve
        # Calculate ticks for x
        axis.x.range.original <- range(dgrafgg$weeks)
        axis.x.otick <- mem:::optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 15, i.valid.ticks = seq_len(5), i.include.min = TRUE, i.include.max = TRUE)
        axis.x.range <- axis.x.otick$range
        axis.x.ticks <- axis.x.otick$tickmarks
        axis.x.labels <- axis.x.otick$tickmarks
        # Range y fix
        axis.y.range.original <- range(dgrafgg$slope)
        axis.y.otick <- mem:::optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
        axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range)) * 0.025 * c(-1, 1)
        axis.y.ticks <- axis.y.otick$tickmarks
        axis.y.labels <- axis.y.otick$tickmarks
        x.opt <- timdata$optimum.map[1]
        y.opt <- dgrafgg$slope[dgrafgg$week == x.opt]
        dgrafgg$slope <- round(dgrafgg$slope, 2)
        gplot <- ggplot(dgrafgg) +
          geom_line(aes(x = weeks, y = slope), color = i.colObservedLines, linetype = 1, size = 1) +
          geom_point(aes(x = weeks, y = slope), color = i.colObservedPoints, size = 3, shape = 21, fill = i.colObservedPoints, stroke = 0.1) +
          geom_hline(yintercept = timdata$param.param, col = i.colLine1, lwd = 1.5, linetype = 2) +
          geom_segment(aes(x = x.opt, y = 0, xend = x.opt, yend = max(dgrafgg$slope, na.rm = TRUE)), col = i.colLine2, lwd = 1) +
          geom_segment(aes(x = min(dgrafgg$weeks), y = y.opt, xend = max(dgrafgg$weeks), yend = y.opt), col = i.colLine2, lwd = 1) +
          geom_point(aes(x = x.opt, y = y.opt), color = i.colOptimum, size = 4, shape = 21, fill = i.colOptimum) +
          scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
          scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
          labs(title = i.textMain, x = i.textX, y = i.textY) +
          theme_light() +
          theme(plot.title = element_text(hjust = 0.5))
        p <- list(plot = gplot, gdata = dgrafgg)
      } else if (i.method == 3) {
        timdata <- memtiming(i.data, i.method = i.method, i.param = i.param)
        dgrafgg <- timdata$slope.curve
        pendiente <- timdata$slope.threshold
        # Calculate ticks for x
        axis.x.range.original <- range(dgrafgg$weeks)
        axis.x.otick <- mem:::optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 15, i.valid.ticks = seq_len(5), i.include.min = TRUE, i.include.max = TRUE)
        axis.x.range <- axis.x.otick$range
        axis.x.ticks <- axis.x.otick$tickmarks
        axis.x.labels <- axis.x.otick$tickmarks
        # Range y fix
        axis.y.range.original <- range(dgrafgg$slope)
        axis.y.otick <- mem:::optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
        axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range)) * 0.025 * c(-1, 1)
        axis.y.ticks <- axis.y.otick$tickmarks
        axis.y.labels <- axis.y.otick$tickmarks
        x.opt <- timdata$optimum.map[1]
        y.opt <- dgrafgg$slope[dgrafgg$week == x.opt]
        b <- diff(range(dgrafgg$slope)) / diff(range(dgrafgg$weeks))
        a1 <- dgrafgg$slope[x.opt] - b * x.opt
        a2 <- dgrafgg$slope[1] - b * 1
        a3 <- dgrafgg$slope[x.opt] + b * x.opt
        dgrafgg$slope <- round(dgrafgg$slope, 2)
        gplot <- ggplot(dgrafgg) +
          geom_line(aes(x = weeks, y = slope), color = i.colObservedLines, linetype = 1, size = 1) +
          geom_point(aes(x = weeks, y = slope), color = i.colObservedPoints, size = 3, shape = 21, fill = i.colObservedPoints, stroke = 0.1) +
          geom_hline(yintercept = pendiente, col = i.colLine1, lwd = 1.5, linetype = 2) +
          geom_segment(aes(x = x.opt, y = 0, xend = x.opt, yend = max(dgrafgg$slope, na.rm = TRUE)), col = i.colLine2, lwd = 1) +
          geom_segment(aes(x = min(dgrafgg$weeks), y = y.opt, xend = max(dgrafgg$weeks), yend = y.opt), col = i.colLine2, lwd = 1) +
          geom_point(aes(x = x.opt, y = y.opt), color = i.colOptimum, size = 4, shape = 21, fill = i.colOptimum) +
          scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
          scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
          labs(title = i.textMain, x = i.textX, y = i.textY) +
          theme_light() +
          theme(plot.title = element_text(hjust = 0.5))
        p <- list(plot = gplot, gdata = dgrafgg)
      } else if (i.method == 4) {
        timdata <- memtiming(i.data, i.method = i.method, i.param = i.param)
        dgrafgg <- timdata$slope.curve
        # Calculate ticks for x
        axis.x.range.original <- range(dgrafgg$weeks)
        axis.x.otick <- mem:::optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 15, i.valid.ticks = seq_len(5), i.include.min = TRUE, i.include.max = TRUE)
        axis.x.range <- axis.x.otick$range
        axis.x.ticks <- axis.x.otick$tickmarks
        axis.x.labels <- axis.x.otick$tickmarks
        # Range y fix
        axis.y.range.original <- range(dgrafgg$slope)
        axis.y.otick <- mem:::optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
        axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range)) * 0.025 * c(-1, 1)
        axis.y.ticks <- axis.y.otick$tickmarks
        axis.y.labels <- axis.y.otick$tickmarks
        x.opt <- timdata$optimum.map[1]
        y.opt <- dgrafgg$slope[dgrafgg$week == x.opt]
        dgrafgg$slope <- round(dgrafgg$slope, 2)
        gplot <- ggplot(dgrafgg) +
          geom_line(aes(x = weeks, y = slope), color = i.colObservedLines, linetype = 1, size = 1) +
          geom_point(aes(x = weeks, y = slope), color = i.colObservedPoints, size = 3, shape = 21, fill = i.colObservedPoints, stroke = 0.1) +
          geom_hline(yintercept = 0, col = i.colLine1, lwd = 1.5, linetype = 2) +
          geom_point(aes(x = x.opt, y = y.opt), color = i.colOptimum, size = 4, shape = 21, fill = i.colOptimum) +
          scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
          scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
          labs(title = i.textMain, x = i.textX, y = i.textY) +
          theme_light() +
          theme(plot.title = element_text(hjust = 0.5))
        p <- list(plot = gplot, gdata = dgrafgg)
      } else {
        p <- NULL
      }
    }
    p
  }

  #####################################
  ### REACTIVE FUNCTIONS
  #####################################

  data_model <- reactive({
    readdata <- read_data()
    cat("reactive/data_model> begin\n")
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      epi <- NULL
    } else {
      # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
      selectedcolumns <- selectColumns(
        i.names = names(datfile),
        i.from = input$SelectFrom,
        i.to = input$SelectTo,
        i.exclude = input$SelectExclude,
        i.include = "",
        i.pandemic = TRUE,
        i.seasons = as.numeric(input$SelectMaximum)
      )

      if (length(selectedcolumns) < 2) {
        temp1 <- memmodel(cbind(datfile[selectedcolumns], datfile[selectedcolumns]),
          i.seasons = NA,
          i.type.threshold = as.numeric(input$typethreshold),
          i.tails.threshold = as.numeric(input$ntails),
          i.type.intensity = as.numeric(input$typeintensity),
          i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
          i.tails.intensity = as.numeric(input$ntails),
          i.type.curve = as.numeric(input$typeaveragecurve),
          i.level.curve = as.numeric(input$levelaveragecurve) / 100,
          i.type.other = as.numeric(input$typeother),
          i.level.other = as.numeric(input$levelother) / 100,
          i.method = as.numeric(input$method),
          i.param = as.numeric(input$param),
          i.centering = as.numeric(input$centering),
          i.n.max = as.numeric(input$nvalues),
          i.use.t = as.logical(input$usetdistribution)
        )
        epi <- list()
        epi$epidemic.thresholds <- temp1$epidemic.thresholds
        epi$intensity.thresholds <- temp1$intensity.thresholds
      } else {
        epi <- memmodel(datfile[selectedcolumns],
          i.seasons = as.numeric(input$SelectMaximum),
          i.type.threshold = as.numeric(input$typethreshold),
          i.tails.threshold = as.numeric(input$ntails),
          i.type.intensity = as.numeric(input$typeintensity),
          i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
          i.tails.intensity = as.numeric(input$ntails),
          i.type.curve = as.numeric(input$typeaveragecurve),
          i.level.curve = as.numeric(input$levelaveragecurve) / 100,
          i.type.other = as.numeric(input$typeother),
          i.level.other = as.numeric(input$levelother) / 100,
          i.method = as.numeric(input$method),
          i.param = as.numeric(input$param),
          i.centering = as.numeric(input$centering),
          i.n.max = as.numeric(input$nvalues),
          i.use.t = as.logical(input$usetdistribution)
        )
      }
    }
    cat("reactive/data_model> end\n")
    epi
  })

  data_good_model <- reactive({
    readdata <- read_data()
    cat("reactive/data_good_model> begin\n")
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      good <- NULL
    } else {
      selectedcolumns <- selectColumns(
        i.names = names(datfile), i.from = input$SelectFrom, i.to = input$SelectTo,
        i.exclude = input$SelectExclude, i.include = "",
        i.pandemic = TRUE,
        i.seasons = as.numeric(input$SelectMaximum)
      )
      if (length(selectedcolumns) < 3) {
        good <- NULL
      } else {
        tfile <- tempfile()
        tfile.div <- extractPfe(tfile)
        good <- memgoodness(datfile[selectedcolumns],
          i.graph = as.logical(input$advanced), i.prefix = tfile.div$name, i.output = tfile.div$path,
          i.min.seasons = 3,
          i.seasons = as.numeric(input$SelectMaximum),
          i.type.threshold = as.numeric(input$typethreshold),
          i.tails.threshold = as.numeric(input$ntails),
          i.type.intensity = as.numeric(input$typeintensity),
          i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
          i.tails.intensity = as.numeric(input$ntails),
          i.type.curve = as.numeric(input$typeaveragecurve),
          i.level.curve = as.numeric(input$levelaveragecurve) / 100,
          i.type.other = as.numeric(input$typeother),
          i.level.other = as.numeric(input$levelother) / 100,
          i.method = as.numeric(input$method),
          i.param = as.numeric(input$param),
          i.detection.values = seq(input$paramrange[1], input$paramrange[2], by = 0.1),
          i.n.max = as.numeric(input$nvalues),
          i.goodness.method = as.character(input$validation)
        )
      }
    }
    # Update goodness graphs tabs
    no.seasons <- NCOL(good$param.data)
    if (good$param.goodness.method == "sequential") se.seasons <- 3:no.seasons else se.seasons <- seq_len(no.seasons)
    nu.seasons <- (seq_len(no.seasons))[se.seasons]
    na.seasons <- (names(good$param.data))[se.seasons]
    lapply(data.frame(rbind(nu.seasons, na.seasons)), function(s) {
      output[[paste0("tbmGoodnessGraphs_", as.character(s[2]))]] <- renderImage(
        {
          graph.file <- paste(good$param.output, "/", good$param.prefix, " Goodness ", s[1], " (", format(round(input$param, 1), digits = 3, nsmall = 1), ").png", sep = "")
          if (!file.exists(graph.file)) {
            gfile <- NULL
          } else {
            gfile <- list(src = graph.file, contentType = "image/png", width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1), alt = "No image found")
          }
          gfile
        },
        deleteFile = FALSE
      )
    })
    cat("reactive/data_good_model> end\n")
    good
  })

  data_good_global <- reactive({
    readdata <- read_data()
    cat("reactive/data_good_global> begin\n")
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      good <- NULL
    } else {
      seasons <- names(datfile)
      selectedcolumns <- selectColumns(
        i.names = seasons,
        i.from = head(seasons, 1),
        i.to = tail(seasons, 1),
        i.exclude = input$SelectExclude,
        i.include = "",
        i.pandemic = TRUE,
        i.seasons = NA
      )
      if (length(selectedcolumns) < 3) {
        good <- NULL
      } else {
        tfile <- tempfile()
        tfile.div <- extractPfe(tfile)
        good <- memgoodness(datfile[selectedcolumns],
          i.graph = as.logical(input$advanced), i.prefix = tfile.div$name, i.output = tfile.div$path,
          i.min.seasons = 3,
          i.seasons = as.numeric(input$SelectMaximum),
          i.type.threshold = as.numeric(input$typethreshold),
          i.tails.threshold = as.numeric(input$ntails),
          i.type.intensity = as.numeric(input$typeintensity),
          i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
          i.tails.intensity = as.numeric(input$ntails),
          i.type.curve = as.numeric(input$typeaveragecurve),
          i.level.curve = as.numeric(input$levelaveragecurve) / 100,
          i.type.other = as.numeric(input$typeother),
          i.level.other = as.numeric(input$levelother) / 100,
          i.method = as.numeric(input$method),
          i.param = as.numeric(input$param),
          i.detection.values = seq(input$paramrange[1], input$paramrange[2], by = 0.1),
          i.n.max = as.numeric(input$nvalues),
          i.goodness.method = as.character(input$validation),
          i.calculation.method = "threshold"
        )
      }
    }
    # Update goodness graphs tabs
    no.seasons <- NCOL(good$param.data)
    if (good$param.goodness.method == "sequential") se.seasons <- 3:no.seasons else se.seasons <- seq_len(no.seasons)
    nu.seasons <- (seq_len(no.seasons))[se.seasons]
    na.seasons <- (names(good$param.data))[se.seasons]
    lapply(data.frame(rbind(nu.seasons, na.seasons)), function(s) {
      output[[paste0("tbdGoodnessGraphs_", as.character(s[2]))]] <- renderImage(
        {
          graph.file <- paste(good$param.output, "/", good$param.prefix, " Goodness ", s[1], " (", format(round(input$param, 1), digits = 3, nsmall = 1), ").png", sep = "")
          if (!file.exists(graph.file)) {
            gfile <- NULL
          } else {
            gfile <- list(src = graph.file, contentType = "image/png", width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1), alt = "No image found")
          }
          gfile
        },
        deleteFile = FALSE
      )
    })
    cat("reactive/data_good_global> end\n")
    good
  })

  data_optim <- reactive({
    readdata <- read_data()
    cat("reactive/data_optim> begin\n")
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      roca <- NULL
    } else {
      selectedcolumns <- selectColumns(
        i.names = names(datfile), i.from = input$SelectFrom, i.to = input$SelectTo,
        i.exclude = input$SelectExclude, i.include = "",
        i.pandemic = TRUE,
        i.seasons = as.numeric(input$SelectMaximum)
      )
      if (length(selectedcolumns) < 3) {
        roca <- NULL
      } else {
        roca <- roc.analysis(datfile[selectedcolumns],
          i.param.values = seq(input$paramrange[1], input$paramrange[2], by = 0.1),
          i.min.seasons = 3,
          i.graph = FALSE,
          i.graph.file = FALSE,
          i.seasons = as.numeric(input$SelectMaximum),
          i.type.threshold = as.numeric(input$typethreshold),
          i.tails.threshold = as.numeric(input$ntails),
          i.type.intensity = as.numeric(input$typeintensity),
          i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
          i.tails.intensity = as.numeric(input$ntails),
          i.type.curve = as.numeric(input$typeaveragecurve),
          i.level.curve = as.numeric(input$levelaveragecurve) / 100,
          i.type.other = as.numeric(input$typeother),
          i.level.other = as.numeric(input$levelother) / 100,
          i.detection.values = seq(input$paramrange[1], input$paramrange[2], by = 0.1),
          i.n.max = as.numeric(input$nvalues),
          i.goodness.method = as.character(input$validation),
          i.min.sensitivity = as.numeric(input$minsensitivity),
          i.min.specificity = as.numeric(input$minspecificity)
        )
      }
    }
    cat("reactive/data_optim> end\n")
    roca
  })

  data_evolution <- reactive({
    readdata <- read_data()
    cat("reactive/data_evolution> begin\n")
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      evo <- NULL
    } else if (NCOL(datfile) < 2) {
      evo <- NULL
    } else {
      evo <- memevolution(
        i.data = datfile,
        i.evolution.seasons = as.numeric(input$SelectMaximum),
        i.evolution.method = as.character(input$validation),
        i.type.threshold = as.numeric(input$typethreshold),
        i.tails.threshold = as.numeric(input$ntails),
        i.type.intensity = as.numeric(input$typeintensity),
        i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
        i.tails.intensity = as.numeric(input$ntails),
        i.type.curve = as.numeric(input$typeaveragecurve),
        i.level.curve = as.numeric(input$levelaveragecurve) / 100,
        i.type.other = as.numeric(input$typeother),
        i.level.other = as.numeric(input$levelother) / 100,
        i.method = as.numeric(input$method),
        i.param = as.numeric(input$param),
        i.n.max = as.numeric(input$nvalues)
      )
    }
    cat("reactive/data_evolution> end\n")
    evo
  })

  data_stability <- reactive({
    readdata <- read_data()
    cat("reactive/data_stability> begin\n")
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      sta <- NULL
    } else if (NCOL(datfile) < 2) {
      sta <- NULL
    } else {
      sta <- memstability(
        i.data = datfile,
        i.type.threshold = as.numeric(input$typethreshold),
        i.tails.threshold = as.numeric(input$ntails),
        i.type.intensity = as.numeric(input$typeintensity),
        i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
        i.tails.intensity = as.numeric(input$ntails),
        i.type.curve = as.numeric(input$typeaveragecurve),
        i.level.curve = as.numeric(input$levelaveragecurve) / 100,
        i.type.other = as.numeric(input$typeother),
        i.level.other = as.numeric(input$levelother) / 100,
        i.method = as.numeric(input$method),
        i.param = as.numeric(input$param),
        i.n.max = as.numeric(input$nvalues)
      )
    }
    cat("reactive/data_stability> end\n")
    sta
  })

  read_data <- reactive({
    infile <- input$file
    indataset <- input$dataset
    inname <- infile$name
    datalog <- character()
    cat("reactive/read_data> begin\n")
    i.range.x <- rep(NA, 2)
    if (!is.null(input$firstWeek)) i.range.x[1] <- as.numeric(input$firstWeek)
    if (!is.null(input$lastWeek)) i.range.x[2] <- as.numeric(input$lastWeek)
    cat("reactive/read_data> Name: ", inname, "\n")
    cat("reactive/read_data> Dataset: ", indataset, "\n")
    cat("reactive/read_data> Range: ", i.range.x[1], "-", i.range.x[2], "\n")
    plots <- NULL
    if (is.null(infile)) {
      datasets <- NULL
      datasetread <- NULL
      datalog <- paste0(datalog, "No file\n")
      cat("reactive/read_data> Warning: No file\n")
    } else if (is.null(indataset)) {
      temp1 <- importData(i.file = infile$datapath, i.file.name = inname, i.range.x = i.range.x, i.process.data = TRUE)
      datasets <- temp1$datasets
      datasetread <- temp1$datasetread
      datalog <- paste0(datalog, temp1$datalog)
      rm("temp1")
      datalog <- paste0(datalog, "No dataset\n")
      cat("reactive/read_data> Warning: No dataset\n")
    } else if (indataset == "") {
      temp1 <- importData(i.file = infile$datapath, i.file.name = inname, i.range.x = i.range.x, i.process.data = TRUE)
      datasets <- temp1$datasets
      datasetread <- temp1$datasetread
      datalog <- paste0(datalog, temp1$datalog)
      rm("temp1")
      datalog <- paste0(datalog, "No dataset\n")
      cat("reactive/read_data> Warning: No dataset\n")
    } else {
      datalog <- paste0(datalog, "Note: reading original data\n")
      cat("reactive/read_data> Note: reading original data\n")
      temp2 <- importData(i.file = infile$datapath, i.file.name = inname, i.dataset = indataset, i.range.x = i.range.x, i.process.data = TRUE)
      datasets <- temp2$datasets
      datasetread <- temp2$datasetread
      datalog <- paste0(datalog, temp2$datalog)
    }
    if (!is.null(datasetread)) {
      dataweeksoriginal <- row.names(temp2$datasetread)
      dataweeksfiltered <- row.names(datasetread)
      if (as.logical(input$processdata)) {
        datalog <- paste0(datalog, "Note: preprocessing activated, data will be checked and rearranged\n")
        cat("reactive/read_data> Note: preprocessing activated, data will be checked and rearranged\n")
        # Delete all columns with only 0s and NAs, it is possible that when rearranging x.range it produces 0's or NA's columns that will give errors afterwards
        zerocols <- apply(datasetread, 2, function(x) sum(x, na.rm = TRUE) == 0)
        if (any(zerocols)) {
          datalog <- paste0(datalog, "Note: removing zero data columns from the original file after rearrangement: ", paste0(names(datasetread)[zerocols], collapse = "; "), "\n")
          cat("reactive/read_data> Note: removing zero data columns from the original file after rearrangement:", paste0(names(datasetread)[zerocols], collapse = ";"), "\n")
          datasetread <- datasetread[!zerocols]
        }
        # Transformation
        if (as.numeric(input$transformation) == 2) {
          datalog <- paste0(datalog, "Note: applying selected transformation - Odd\n")
          cat("reactive/read_data> Note: applying selected transformation - Odd\n")
          datasetread <- transformseries(datasetread, i.transformation = as.numeric(input$transformation))
        } else if (as.numeric(input$transformation) == 3) {
          datalog <- paste0(datalog, "Note: applying selected transformation - Fill missings\n")
          cat("reactive/read_data> Note: applying selected transformation - Fill missings\n")
          datasetread <- transformseries(datasetread, i.transformation = as.numeric(input$transformation))
        } else if (as.numeric(input$transformation) == 4) {
          datalog <- paste0(datalog, "Note: applying selected transformation - Smoothing regression\n")
          cat("reactive/read_data> Note: applying selected transformation - Smoothing regression\n")
          if (input$smregressionoptimum) hsuav.value <- -1 else hsuav.value <- as.numeric(input$smregressionsmoothing)
          datasetread <- transformseries(datasetread, i.transformation = 4, hsuav = hsuav.value)
        } else if (as.numeric(input$transformation) == 5) {
          datalog <- paste0(datalog, "Note: applying selected transformation - Loess\n")
          cat("reactive/read_data> Note: applying selected transformation - Loess\n")
          datasetread <- transformseries(datasetread, i.transformation = 7, i.positive = as.logical(input$transfpositive), span = as.numeric(input$loesspan))
        } else if (as.numeric(input$transformation) == 6) {
          datalog <- paste0(datalog, "Note: applying selected transformation - Spline\n")
          cat("reactive/read_data> Note: applying selected transformation - Spline\n")
          datasetread <- transformseries(datasetread, i.transformation = 8, i.positive = as.logical(input$transfpositive))
        } else if (as.numeric(input$transformation) == 7) {
          datalog <- paste0(datalog, "Note: applying selected transformation - Moving average\n")
          cat("reactive/read_data> Note: applying selected transformation - Moving average\n")
          datasetread <- transformseries(datasetread, i.transformation = 9, i.number = as.numeric(input$movavgweeks))
        } else {
          datalog <- paste0(datalog, "Note: no transformation selected\n")
          cat("reactive/read_data> Note: no transformation selected\n")
        }
        # Waves separation
        if (as.numeric(input$waves) == 2) {
          datalog <- paste0(datalog, "Note: separating waves\n")
          cat("reactive/read_data> Note: separating waves\n")
          datasetread <- transformseries(datasetread, i.transformation = 5, i.proportion = as.numeric(input$twowavesproportion / 100))
        } else if (as.numeric(input$waves) == 3) {
          datalog <- paste0(datalog, "Note: separating waves\n")
          cat("reactive/read_data> Note: separating waves\n")
          datasetread <- transformseries(datasetread, i.transformation = 6, i.proportion = as.numeric(input$twowavesproportion / 100))
        } else if (as.numeric(input$waves) == 4) {
          datalog <- paste0(datalog, "Note: separating waves\n")
          cat("reactive/read_data> Note: separating waves\n")
          # If I use the i.method and i.param parameters when I produce the graph of multiple
          # waves, it modifies this the graph I use (that shows the epidemic period as shown
          # by the algorithm) and then consequently read_data changes and reset the information
          # shown at Model. So if I use multiple and change the i.param then it is reset the
          # Model selection, which I do not want it to happen.
          # Calcular el número de puntos que se toman
          temp1 <- mem:::transformseries.multiple(datasetread,
            i.waves = as.numeric(input$numberwaves),
            i.min.separation = as.numeric(input$wavesseparation),
            i.intra.param = as.numeric(input$wavesparam1),
            i.inter.param = as.numeric(input$wavesparam2),
            i.method = as.numeric(input$method),
            i.param = as.numeric(input$param),
            i.p3titles = c(trloc("selections.dataset.wavesdetection.multiple.iteration"), trloc("selections.surveillance.week.label"), trloc("main.checkdescribe.data")),
            i.p4titles = c(trloc("selections.dataset.wavesdetection.multiple.merged"), trloc("selections.dataset.wavesdetection.multiple.separation.label"), trloc("selections.dataset.wavesdetection.multiple.iteration"), trloc("selections.surveillance.week.label"), trloc("main.checkdescribe.data")),
            i.p5titles = c(trloc("selections.dataset.wavesdetection.multiple.artificial"), trloc("selections.dataset.wavesdetection.multiple.memepidemics"), trloc("selections.surveillance.week.label"), trloc("main.checkdescribe.data"))
          )
          datalog <- paste0(datalog, "Note: Description of dummy seasons created\n\t", trloc("selections.surveillance.season.label"), "\t", trloc("selections.model.from.label"), "\t", trloc("selections.model.to.label"), "\n", paste0(apply(temp1$season.desc, 1, function(x) paste0("\t", paste0(as.character(x), collapse = "\t"))), collapse = "\n"))
          datasetread <- temp1$data.final
          plots <- temp1$plots
          rm("temp1")
        } else {
          datalog <- paste0(datalog, "Note: no separation of waves selected\n")
          cat("reactive/read_data> Note: no separation of waves selected\n")
        }
        # Delete all columns with only 0s and NAs. After transformation is possible that some columns are NA,
        # specially when splitting waves in two, in case there is only one epidemic instead of two
        zerocols <- apply(datasetread, 2, function(x) sum(x, na.rm = TRUE) == 0)
        if (sum(zerocols) > 0) {
          datalog <- paste0(datalog, "Note: removing 0/NA-only columns after transformation:", paste0(names(datasetread)[zerocols], collapse = ";"), "\n")
          cat("reactive/read_data> Note: removing 0/NA-only columns after transformation:", paste0(names(datasetread)[zerocols], collapse = ";"), "\n")
          datasetread <- datasetread[!zerocols]
        }
      } else {
        datalog <- paste0(datalog, "Note: preprocessing deactivated, data will be read as it is\n")
        cat("reactive/read_data> Note: preprocessing deactivated, data will be read as it is\n")
      }
    } else {
      dataweeksoriginal <- NULL
      dataweeksfiltered <- NULL
    }
    cat("reactive/read_data> datasets returning NULL?: ", is.null(datasets), "\n")
    cat("reactive/read_data> dataweeksoriginal returning NULL?: ", is.null(dataweeksoriginal), "\n")
    cat("reactive/read_data> dataweeksfiltered returning NULL?: ", is.null(dataweeksfiltered), "\n")
    cat("reactive/read_data> datasetread NULL?: ", is.null(datasetread), "\n")
    cat("reactive/read_data> end\n")
    readdata <- list(datasets = datasets, datasetread = datasetread, dataweeksoriginal = dataweeksoriginal, dataweeksfiltered = dataweeksfiltered, datalog = datalog, plots = plots)
    readdata
  })

  getSeasons <- reactive({
    cat("reactive/getSeasons> begin\n")
    readdata <- read_data()
    datfile <- readdata$datasetread
    seasons <- names(datfile)
    if (!is.null(seasons)) cat("reactive/getSeasons> updating from/to/exclude\n")
    cat("reactive/getSeasons> end\n")
    return(seasons)
  })

  getDatasets <- eventReactive(input$file, {
    cat("eventReactive/getDatasets> begin\n")
    readdata <- read_data()
    datsheets <- readdata$datasets
    if (!is.null(datsheets)) cat("eventReactive/getDatasets> updating dataset list\n")
    cat("eventReactive/getDatasets> end\n")
    return(datsheets)
  })

  getWeeksOriginal <- eventReactive(input$dataset, {
    cat("eventReactive/getWeeksOriginal> begin\n")
    readdata <- read_data()
    dataweeksoriginal <- readdata$dataweeksoriginal
    if (!is.null(dataweeksoriginal)) cat("reactive/getWeeksOriginal> updating first/last week list\n")
    cat("eventReactive/getWeeksOriginal> end\n")
    return(dataweeksoriginal)
  })

  getWeeksFiltered <- eventReactive(c(input$dataset, input$firstWeek, input$lastWeek), {
    cat("eventReactive/getWeeksFiltered> begin\n")
    readdata <- read_data()
    dataweeksfiltered <- readdata$dataweeksfiltered
    if (!is.null(dataweeksfiltered)) cat("reactive/getWeeksFiltered> updating first/last week list\n")
    cat("eventReactive/getWeeksFiltered> end\n")
    return(dataweeksfiltered)
  })

  #####################################
  ### OBSERVERS
  #####################################
  # Pass url parameters to the app, in this case to advanced features, once the server is run, you can
  # use http://127.0.0.1:7910/?advanced=TRUE to enable/disable advanced features

  observe({
    cat("observe/urlquery> begin\n")
    query <- parseQueryString(session$clientData$url_search)
    cat("observe/urlquery> searching for advanced features URL parameter\n")
    if (!is.null(query[["advanced"]])) {
      cat("observe/urlquery> setting advanced features to ", query[["advanced"]], "\n")
      updatePrettyCheckbox(session, "advanced", value = as.logical(query[["advanced"]]))
    }
    cat("observe/urlquery> searching for experimental features URL parameter\n")
    if (!is.null(query[["experimental"]])) {
      cat("observe/urlquery> setting experimental features to ", query[["experimental"]], "\n")
      updatePrettyCheckbox(session, "experimental", value = as.logical(query[["experimental"]]))
    }
    cat("observe/urlquery> searching for language URL parameter\n")
    if (!is.null(query[["language"]])) {
      cat("observe/urlquery> setting language to ", query[["language"]], "\n")
      updateSelectInput(session, "language", selected = as.character(query[["language"]]))
    }
    cat("observe/urlquery> begin\n")
  })

  observeEvent(input$language, {
    lang <- input$language
    cat("observeEvent/language> begin\n")
    cat("observeEvent/language> original locale:", values$locale, "\n")
    langs <- getLanguages()
    if (lang %in% langs$filename) {
      if (.Platform$OS.type == "windows") {
        cat("observeEvent/language> Windows system detected\n")
        localestring <- langs$localewin[langs$filename == lang]
      } else if (.Platform$OS.type == "unix") {
        cat("observeEvent/language> *nix system detected\n")
        localestring <- langs$localelinux[langs$filename == lang]
        if (localestring == "") cat("observeEvent/language> Locale not installed in your system\n")
      } else {
        cat("observeEvent/language> No windows or *nix system detected\n")
        localestring <- ""
      }
      cat("observeEvent/language> changing to:", dplyr::if_else(localestring == "", "system default", localestring), "\n")
      Sys.setlocale(locale = localestring)
    } else {
      cat("observeEvent/language> language not in the locales list\n")
    }
    cat("observeEvent/language> current locale:", Sys.getlocale(), "\n")
    cat("observeEvent/language> end\n")
  })

  observeEvent(input$dataset, {
    lang <- input$language
    cat("observeEvent/dataset> begin\n")
    cat("observeEvent/dataset> setting to default values\n")
    updateMaterialSwitch(session, "processdata", value = default_values$processdata)
    updatePrettyCheckbox(session, "preepidemicthr", value = default_values$preepidemicthr)
    updatePrettyCheckbox(session, "postepidemicthr", value = default_values$postepidemicthr)
    updatePrettyCheckbox(session, "intensitythr", value = default_values$intensitythr)
    updatePrettyCheckbox(session, "plottiming", value = default_values$plottiming)
    updateSelectInput(session, "transformation", selected = default_values$transformation)
    updatePickerInput(session, "SelectSeasons", selected = NULL)
    updatePickerInput(session, "SelectExclude", selected = NULL)
    if (input$transformation == 5 & input$advanced) updateSliderInput(session, "loesspan", value = default_values$loesspan$value, min = default_values$loesspan$min, max = default_values$loesspan$max, step = default_values$loesspan$step)
    if (input$transformation == 7 & input$advanced) updateSliderInput(session, "movavgweeks", value = default_values$movavgweeks$value, min = default_values$movavgweeks$min, max = default_values$movavgweeks$max, step = default_values$movavgweeks$step)
    if (input$transformation == 4 & input$advanced) updatePrettyCheckbox(session, "smregressionoptimum", value = default_values$smregressionoptimum)
    if (input$transformation == 4 & input$advanced) updateSliderInput(session, "smregressionsmoothing", value = default_values$smregressionsmoothing$value, min = default_values$smregressionsmoothing$min, max = default_values$smregressionsmoothing$max, step = default_values$smregressionsmoothing$step)
    if ((input$transformation == 5 | input$transformation == 6) & input$advanced) updatePrettyCheckbox(session, "transfpositive", value = default_values$transfpositive)
    updateSelectInput(session, "waves", selected = default_values$waves)
    if ((input$waves == 2 | input$waves == 3) & input$advanced) updateSliderInput(session, "twowavesproportion", min = default_values$twowavesproportion$min, max = default_values$twowavesproportion$max, step = default_values$twowavesproportion$step, value = default_values$twowavesproportion$value)
    if (input$waves == 4 & input$experimental & input$advanced) {
      updateNumericInput(session, "numberwaves", min = default_values$numberwaves$min, max = default_values$numberwaves$max, step = default_values$numberwaves$step, value = default_values$numberwaves$value)
      updateNumericInput(session, "wavesseparation", min = default_values$wavesseparation$min, max = default_values$wavesseparation$max, step = default_values$wavesseparation$step, value = default_values$wavesseparation$value)
      updateNumericInput(session, "wavesparam1", min = default_values$wavesparam1$min, max = default_values$wavesparam1$max, step = default_values$wavesparam1$step, value = default_values$wavesparam1$value)
      updateNumericInput(session, "wavesparam2", min = default_values$wavesparam2$min, max = default_values$wavesparam2$max, step = default_values$wavesparam2$step, value = default_values$wavesparam2$value)
    }
    # Text options
    updateTextInput(session, "textMain", value = trloc("options.text.main.label"))
    updateTextInput(session, "textY", value = trloc("options.text.yaxis.label"))
    updateTextInput(session, "textX", value = trloc("options.text.xaxis.label"))
    # Graph options
    updateSelectInput(session, "colObservedLines", selected = default_values$colObservedLines)
    updateSelectInput(session, "colObservedPoints", selected = default_values$colObservedPoints)
    updateSelectInput(session, "colEpidemicStart", selected = default_values$colEpidemicStart)
    updateSelectInput(session, "colEpidemicStop", selected = default_values$colEpidemicStop)
    updateSelectInput(session, "colThresholds", selected = default_values$colThresholds)
    updateSelectInput(session, "colLevels", selected = default_values$colLevels)
    updateSelectInput(session, "colSeasons", selected = default_values$colSeasons)
    updateSelectInput(session, "colEpidemic", selected = default_values$colEpidemic)
    updatePrettyCheckbox(session, "yaxis0", value = default_values$yaxis0)
    # MEM options
    updateSelectInput(session, "method", selected = default_values$method)
    updateNumericInput(session, "param", value = default_values$param$value, min = default_values$param$min, max = default_values$param$max, step = default_values$param$step)
    updateNumericInput(session, "optimparam", value = default_values$optimparam$value, min = default_values$optimparam$min, max = default_values$optimparam$max, step = default_values$optimparam$step)
    updateSelectInput(session, "nvalues", selected = default_values$nvalues)
    updateNumericInput(session, "ntails", value = default_values$ntails$value, min = default_values$ntails$min, max = default_values$ntails$max, step = default_values$ntails$step)
    updateSelectInput(session, "typethreshold", selected = default_values$typethreshold)
    updateSelectInput(session, "typeintensity", selected = default_values$typeintensity)
    updateNumericInput(session, "levelintensitym", value = default_values$levelintensitym$value, min = default_values$levelintensitym$min, max = default_values$levelintensitym$max, step = default_values$levelintensitym$step)
    updateNumericInput(session, "levelintensityh", value = default_values$levelintensityh$value, min = default_values$levelintensityh$min, max = default_values$levelintensityh$max, step = default_values$levelintensityh$step)
    updateNumericInput(session, "levelintensityv", value = default_values$levelintensityv$value, min = default_values$levelintensityv$min, max = default_values$levelintensityv$max, step = default_values$levelintensityv$step)
    updateSelectInput(session, "validation", selected = default_values$validation)
    updateSelectInput(session, "optimmethod", selected = default_values$optimmethod)
    updateSliderInput(session, "paramrange", value = default_values$paramrange$value, min = default_values$paramrange$min, max = default_values$paramrange$max, step = default_values$paramrange$step)
    updateNumericInput(session, "minsensitivity", value = default_values$minsensitivity$value, min = default_values$minsensitivity$min, max = default_values$minsensitivity$max, step = default_values$minsensitivity$step)
    updateNumericInput(session, "minspecificity", value = default_values$minspecificity$value, min = default_values$minspecificity$min, max = default_values$minspecificity$max, step = default_values$minspecificity$step)
    updateSelectInput(session, "typeaveragecurve", selected = default_values$typeaveragecurve)
    updateSelectInput(session, "typeother", selected = default_values$typeother)
    updateNumericInput(session, "levelaveragecurve", value = default_values$levelaveragecurve$value, min = default_values$levelaveragecurve$min, max = default_values$levelaveragecurve$max, step = default_values$levelaveragecurve$step)
    updateNumericInput(session, "levelother", value = default_values$levelother$value, min = default_values$levelother$min, max = default_values$levelother$max, step = default_values$levelother$step)
    updateSelectInput(session, "centering", selected = default_values$centering)
    updateMaterialSwitch(session, "usetdistribution", value = default_values$usetdistribution)
    cat("observeEvent/dataset> end\n")
  })

  observeEvent(read_data(), {
    cat("observeEvent/read_data> begin\n")
    readdata <- read_data()
    datfile <- readdata$datasetread
    datsheets <- readdata$datasets
    if (!is.null(datfile)) {
      seasons <- names(datfile)
      cat("observeEvent/read_data> updating timing graphs/structure... check & describe\n")
      lapply(seasons, function(s) {
        output[[paste0("tbdTiming_", as.character(s))]] <- renderUI({
          if (as.logical(input$advanced)) {
            fluidPage(
              fluidRow(
                column(1, h4(trloc("main.checkdescribe.timing"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, plotlyOutput(paste0("tbdTiming_", as.character(s), "_plot"), width = "auto", height = "auto"))
              ),
              fluidRow(
                column(1, h4(trloc("main.checkdescribe.timing.map"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, plotlyOutput(paste0("tbdTiming_", as.character(s), "_map"), width = "auto", height = "auto"))
              ),
              fluidRow(
                column(1, h4(trloc("main.checkdescribe.timing.slope"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, plotlyOutput(paste0("tbdTiming_", as.character(s), "_slope"), width = "auto", height = "auto"))
              )
            )
          } else {
            fluidRow(
              column(1, h4(trloc("main.checkdescribe.timing"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
              column(11, plotlyOutput(paste0("tbdTiming_", as.character(s), "_plot"), width = "auto", height = "auto"))
            )
          }
        })
      })
      cat("observeEvent/read_data> updating timing graphs/timing plot... check & describe\n")
      lapply(seasons, function(s) {
        output[[paste0("tbdTiming_", as.character(s), "_plot")]] <- renderPlotly({
          readdata <- isolate(read_data())
          datfile <- readdata$datasetread
          if (is.null(datfile)) {
            zfix <- NULL
          } else if (!(as.character(s) %in% names(datfile))) {
            zfix <- NULL
          } else {
            datfile.plot <- datfile[as.character(s)]
            colors.palette <- generatePalette(
              i.number.series = NA,
              i.colObservedLines = input$colObservedLines,
              i.colObservedPoints = input$colObservedPoints,
              i.colEpidemicStart = input$colEpidemicStart,
              i.colEpidemicStop = input$colEpidemicStop,
              i.colThresholds = input$colThresholds,
              i.colSeasons = input$colSeasons,
              i.colEpidemic = input$colEpidemic
            )
            p <- plotSeries(datfile.plot,
              i.plot.timing = TRUE,
              i.range.x = NA,
              i.pre.epidemic = FALSE,
              i.post.epidemic = FALSE,
              i.intensity = FALSE,
              i.replace.x.cr = FALSE,
              i.textMain = input$textMain,
              i.textX = input$textX,
              i.textY = input$textY,
              i.type.threshold = as.numeric(input$typethreshold),
              i.tails.threshold = as.numeric(input$ntails),
              i.type.intensity = as.numeric(input$typeintensity),
              i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
              i.tails.intensity = as.numeric(input$ntails),
              i.type.curve = as.numeric(input$typeaveragecurve),
              i.level.curve = as.numeric(input$levelaveragecurve) / 100,
              i.type.other = as.numeric(input$typeother),
              i.level.other = as.numeric(input$levelother) / 100,
              i.method = as.numeric(input$method),
              i.param = as.numeric(input$param),
              i.centering = as.numeric(input$centering),
              i.n.max = as.numeric(input$nvalues),
              i.colObservedLines = colors.palette$colObservedLines,
              i.colThresholds = colors.palette$colThresholds,
              i.colObservedPoints = colors.palette$colObservedPoints,
              i.colEpidemic = colors.palette$colEpidemic,
              i.yaxis.starts.at.0 = as.logical(input$yaxis0),
              i.use.t = as.logical(input$usetdistribution)
            )
            if (is.null(p)) {
              zfix <- NULL
            } else {
              z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
              zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
            }
          }
          zfix
        })
      })
      cat("observeEvent/read_data> updating timing graphs/map curve... check & describe\n")
      lapply(seasons, function(s) {
        output[[paste0("tbdTiming_", as.character(s), "_map")]] <- renderPlotly({
          readdata <- isolate(read_data())
          datfile <- readdata$datasetread
          if (is.null(datfile)) {
            zfix <- NULL
          } else if (!(as.character(s) %in% names(datfile))) {
            zfix <- NULL
          } else {
            datfile.plot <- datfile[as.character(s)]
            colors.palette <- generatePalette(
              i.number.series = NCOL(datfile.plot),
              i.colObservedLines = input$colObservedLines,
              i.colObservedPoints = input$colObservedPoints,
              i.colEpidemicStart = input$colEpidemicStart,
              i.colEpidemicStop = input$colEpidemicStop,
              i.colThresholds = input$colThresholds,
              i.colSeasons = input$colSeasons,
              i.colEpidemic = input$colEpidemic
            )
            p <- plotMAP(datfile.plot,
              i.textMain = input$textMain,
              i.textX = input$textX,
              i.textY = input$textY,
              i.method = as.numeric(input$method),
              i.param = as.numeric(input$param),
              i.colObservedLines = colors.palette$colObservedLines,
              i.colObservedPoints = colors.palette$colObservedPoints,
              i.colOptimum = colors.palette$colEpidemicStart,
              i.colLine = colors.palette$colEpidemic[3]
            )
            if (is.null(p)) {
              zfix <- NULL
            } else {
              z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
              zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
            }
          }
          zfix
        })
      })
      cat("observeEvent/read_data> updating timing graphs/slope curve... check & describe\n")
      lapply(seasons, function(s) {
        output[[paste0("tbdTiming_", as.character(s), "_slope")]] <- renderPlotly({
          readdata <- isolate(read_data())
          datfile <- readdata$datasetread
          if (is.null(datfile)) {
            zfix <- NULL
          } else if (!(as.character(s) %in% names(datfile))) {
            zfix <- NULL
          } else {
            datfile.plot <- datfile[as.character(s)]
            colors.palette <- generatePalette(
              i.number.series = NCOL(datfile.plot),
              i.colObservedLines = input$colObservedLines,
              i.colObservedPoints = input$colObservedPoints,
              i.colEpidemicStart = input$colEpidemicStart,
              i.colEpidemicStop = input$colEpidemicStop,
              i.colThresholds = input$colThresholds,
              i.colSeasons = input$colSeasons,
              i.colEpidemic = input$colEpidemic
            )
            p <- plotSlope(datfile.plot,
              i.textMain = input$textMain,
              i.textX = input$textX,
              i.textY = input$textY,
              i.method = as.numeric(input$method),
              i.param = as.numeric(input$param),
              i.colObservedLines = colors.palette$colObservedLines,
              i.colObservedPoints = colors.palette$colObservedPoints,
              i.colOptimum = colors.palette$colEpidemicStart,
              i.colLine1 = colors.palette$colEpidemic[2],
              i.colLine2 = colors.palette$colEpidemic[3]
            )
            if (is.null(p)) {
              zfix <- NULL
            } else {
              z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
              zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
            }
          }
          zfix
        })
      })
      cat("observeEvent/read_data> updating timing graphs/structure... visualize\n")
      lapply(seasons, function(s) {
        output[[paste0("tbvTiming_", as.character(s))]] <- renderUI({
          if (as.logical(input$advanced)) {
            fluidPage(
              fluidRow(
                column(1, h4(trloc("main.checkdescribe.timing"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, plotlyOutput(paste0("tbvTiming_", as.character(s), "_plot"), width = "auto", height = "auto"))
              ),
              fluidRow(
                column(1, h4(trloc("main.checkdescribe.timing.map"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, plotlyOutput(paste0("tbvTiming_", as.character(s), "_map"), width = "auto", height = "auto"))
              ),
              fluidRow(
                column(1, h4(trloc("main.checkdescribe.timing.slope"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, plotlyOutput(paste0("tbvTiming_", as.character(s), "_slope"), width = "auto", height = "auto"))
              )
            )
          } else {
            fluidRow(
              column(1, h4(trloc("main.checkdescribe.timing"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
              column(11, plotlyOutput(paste0("tbvTiming_", as.character(s), "_plot"), width = "auto", height = "auto"))
            )
          }
        })
      })
      cat("observeEvent/read_data> updating timing graphs/timing plot... visualize\n")
      lapply(seasons, function(s) {
        output[[paste0("tbvTiming_", as.character(s), "_plot")]] <- renderPlotly({
          readdata <- isolate(read_data())
          datfile <- readdata$datasetread
          if (is.null(datfile)) {
            zfix <- NULL
          } else if (!(as.character(s) %in% names(datfile))) {
            zfix <- NULL
          } else {
            datfile.plot <- datfile[as.character(s)]
            colors.palette <- generatePalette(
              i.number.series = NA,
              i.colObservedLines = input$colObservedLines,
              i.colObservedPoints = input$colObservedPoints,
              i.colEpidemicStart = input$colEpidemicStart,
              i.colEpidemicStop = input$colEpidemicStop,
              i.colThresholds = input$colThresholds,
              i.colSeasons = input$colSeasons,
              i.colEpidemic = input$colEpidemic
            )
            p <- plotSeries(datfile.plot,
              i.plot.timing = TRUE,
              i.range.x = NA,
              i.pre.epidemic = FALSE,
              i.post.epidemic = FALSE,
              i.intensity = FALSE,
              i.replace.x.cr = FALSE,
              i.textMain = input$textMain,
              i.textX = input$textX,
              i.textY = input$textY,
              i.type.threshold = as.numeric(input$typethreshold),
              i.tails.threshold = as.numeric(input$ntails),
              i.type.intensity = as.numeric(input$typeintensity),
              i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
              i.tails.intensity = as.numeric(input$ntails),
              i.type.curve = as.numeric(input$typeaveragecurve),
              i.level.curve = as.numeric(input$levelaveragecurve) / 100,
              i.type.other = as.numeric(input$typeother),
              i.level.other = as.numeric(input$levelother) / 100,
              i.method = as.numeric(input$method),
              i.param = as.numeric(input$param),
              i.centering = as.numeric(input$centering),
              i.n.max = as.numeric(input$nvalues),
              i.colObservedLines = colors.palette$colObservedLines,
              i.colThresholds = colors.palette$colThresholds,
              i.colObservedPoints = colors.palette$colObservedPoints,
              i.colEpidemic = colors.palette$colEpidemic,
              i.yaxis.starts.at.0 = as.logical(input$yaxis0),
              i.use.t = as.logical(input$usetdistribution)
            )
            if (is.null(p)) {
              zfix <- NULL
            } else {
              z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
              zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
            }
          }
          zfix
        })
      })
      cat("observeEvent/read_data> updating timing graphs/map curve... visualize\n")
      lapply(seasons, function(s) {
        output[[paste0("tbvTiming_", as.character(s), "_map")]] <- renderPlotly({
          readdata <- isolate(read_data())
          datfile <- readdata$datasetread
          if (is.null(datfile)) {
            zfix <- NULL
          } else if (!(as.character(s) %in% names(datfile))) {
            zfix <- NULL
          } else {
            datfile.plot <- datfile[as.character(s)]
            colors.palette <- generatePalette(
              i.number.series = NCOL(datfile.plot),
              i.colObservedLines = input$colObservedLines,
              i.colObservedPoints = input$colObservedPoints,
              i.colEpidemicStart = input$colEpidemicStart,
              i.colEpidemicStop = input$colEpidemicStop,
              i.colThresholds = input$colThresholds,
              i.colSeasons = input$colSeasons,
              i.colEpidemic = input$colEpidemic
            )

            p <- plotMAP(datfile.plot,
              i.textMain = input$textMain,
              i.textX = input$textX,
              i.textY = input$textY,
              i.method = as.numeric(input$method),
              i.param = as.numeric(input$param),
              i.colObservedLines = colors.palette$colObservedLines,
              i.colObservedPoints = colors.palette$colObservedPoints,
              i.colOptimum = colors.palette$colEpidemicStart,
              i.colLine = colors.palette$colEpidemic[3]
            )
            if (is.null(p)) {
              zfix <- NULL
            } else {
              z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
              zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
            }
          }
          zfix
        })
      })
      cat("observeEvent/read_data> updating timing graphs/slope curve... visualize\n")
      lapply(seasons, function(s) {
        output[[paste0("tbvTiming_", as.character(s), "_slope")]] <- renderPlotly({
          readdata <- isolate(read_data())
          datfile <- readdata$datasetread
          if (is.null(datfile)) {
            zfix <- NULL
          } else if (!(as.character(s) %in% names(datfile))) {
            zfix <- NULL
          } else {
            datfile.plot <- datfile[as.character(s)]
            colors.palette <- generatePalette(
              i.number.series = NCOL(datfile.plot),
              i.colObservedLines = input$colObservedLines,
              i.colObservedPoints = input$colObservedPoints,
              i.colEpidemicStart = input$colEpidemicStart,
              i.colEpidemicStop = input$colEpidemicStop,
              i.colThresholds = input$colThresholds,
              i.colSeasons = input$colSeasons,
              i.colEpidemic = input$colEpidemic
            )
            p <- plotSlope(datfile.plot,
              i.textMain = input$textMain,
              i.textX = input$textX,
              i.textY = input$textY,
              i.method = as.numeric(input$method),
              i.param = as.numeric(input$param),
              i.colObservedLines = colors.palette$colObservedLines,
              i.colObservedPoints = colors.palette$colObservedPoints,
              i.colOptimum = colors.palette$colEpidemicStart,
              i.colLine1 = colors.palette$colEpidemic[2],
              i.colLine2 = colors.palette$colEpidemic[3]
            )
            if (is.null(p)) {
              zfix <- NULL
            } else {
              z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
              zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
            }
          }
          zfix
        })
      })
    }
    cat("observeEvent/read_data> end\n")
  })

  observeEvent(data_model(), {
    cat("observeEvent/data_model> begin\n")
    datamodel <- data_model()
    moddata <- datamodel$param.data
    if (!is.null(moddata)) {
      cat("observeEvent/data_model> updating timing plots... updating global variables\n")
      modseasons <- names(moddata)
      plotdata <- cbind(data.frame(weekno = seq_len(NROW(moddata)), weekna = rownames(moddata), stringsAsFactors = FALSE), moddata)
      epidata <- datamodel$data
      names(epidata) <- paste0(names(epidata), "_fixed")
      epidata$weekna <- rownames(epidata)
      plotdata <- merge(plotdata, epidata, "weekna", all.x = TRUE, sort = FALSE)
      # I have to duplicate the dataset since the plotdata will be changing when I add colors to the _color
      # column, if I use plotdata for detect nearpoint, each time I change a color, it reevaluates the
      # expression nearpoint, producing duplicate points
      origdata <- plotdata
      for (s in modseasons) eval(parse(text = paste0("plotdata$'", as.character(s), "_color'<-'1'")))
      values$origdata <- origdata
      values$plotdata <- plotdata
      values$clickdata <- data.frame()
      values$optimizegraphs <- data.frame()
      rm("origdata", "plotdata", "epidata")
      cat("observeEvent/data_model> updating timing graphs/structure... model\n")
      lapply(modseasons, function(s) {
        output[[paste0("tbmTiming_", as.character(s))]] <- renderUI({
          if (as.logical(input$advanced)) {
            fluidPage(
              fluidRow(
                column(1, h4(trloc("main.checkdescribe.timing"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, plotlyOutput(paste0("tbmTiming_", as.character(s), "_plot"), width = "auto", height = "auto"))
              ),
              fluidRow(
                column(1, h4(trloc("main.checkdescribe.timing.map"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, plotlyOutput(paste0("tbmTiming_", as.character(s), "_map"), width = "auto", height = "auto"))
              ),
              fluidRow(
                column(1, h4(trloc("main.checkdescribe.timing.slope"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, plotlyOutput(paste0("tbmTiming_", as.character(s), "_slope"), width = "auto", height = "auto"))
              )
            )
          } else {
            fluidRow(
              column(1, h4(trloc("main.checkdescribe.timing"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
              column(11, plotlyOutput(paste0("tbmTiming_", as.character(s), "_plot"), width = "auto", height = "auto"))
            )
          }
        })
      })
      cat("observeEvent/data_model> updating timing graphs/timing plot... model\n")
      lapply(modseasons, function(s) {
        output[[paste0("tbmTiming_", as.character(s), "_plot")]] <- renderPlotly({
          datamodel <- isolate(data_model())
          moddata <- datamodel$param.data
          if (is.null(moddata)) {
            zfix <- NULL
          } else if (!(as.character(s) %in% names(moddata))) {
            zfix <- NULL
          } else {
            moddata.s <- moddata[as.character(s)]
            colors.palette <- generatePalette(
              i.number.series = NA,
              i.colObservedLines = input$colObservedLines,
              i.colObservedPoints = input$colObservedPoints,
              i.colEpidemicStart = input$colEpidemicStart,
              i.colEpidemicStop = input$colEpidemicStop,
              i.colThresholds = input$colThresholds,
              i.colSeasons = input$colSeasons,
              i.colEpidemic = input$colEpidemic
            )
            p <- plotSeries(moddata.s,
              i.plot.timing = TRUE,
              i.range.x = NA,
              i.pre.epidemic = FALSE,
              i.post.epidemic = FALSE,
              i.intensity = FALSE,
              i.replace.x.cr = FALSE,
              i.textMain = input$textMain,
              i.textX = input$textX,
              i.textY = input$textY,
              i.type.threshold = as.numeric(input$typethreshold),
              i.tails.threshold = as.numeric(input$ntails),
              i.type.intensity = as.numeric(input$typeintensity),
              i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
              i.tails.intensity = as.numeric(input$ntails),
              i.type.curve = as.numeric(input$typeaveragecurve),
              i.level.curve = as.numeric(input$levelaveragecurve) / 100,
              i.type.other = as.numeric(input$typeother),
              i.level.other = as.numeric(input$levelother) / 100,
              i.method = as.numeric(input$method),
              i.param = as.numeric(input$param),
              i.centering = as.numeric(input$centering),
              i.n.max = as.numeric(input$nvalues),
              i.colObservedLines = colors.palette$colObservedLines,
              i.colThresholds = colors.palette$colThresholds,
              i.colObservedPoints = colors.palette$colObservedPoints,
              i.colEpidemic = colors.palette$colEpidemic,
              i.yaxis.starts.at.0 = as.logical(input$yaxis0),
              i.use.t = as.logical(input$usetdistribution)
            )
            if (is.null(p)) {
              zfix <- NULL
            } else {
              z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
              zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
            }
          }
          zfix
        })
      })
      cat("observeEvent/data_model> updating timing graphs/map curve... model\n")
      lapply(modseasons, function(s) {
        output[[paste0("tbmTiming_", as.character(s), "_map")]] <- renderPlotly({
          readdata <- isolate(read_data())
          datfile <- readdata$datasetread
          if (is.null(datfile)) {
            zfix <- NULL
          } else if (!(as.character(s) %in% names(datfile))) {
            zfix <- NULL
          } else {
            datfile.plot <- datfile[as.character(s)]
            colors.palette <- generatePalette(
              i.number.series = NCOL(datfile.plot),
              i.colObservedLines = input$colObservedLines,
              i.colObservedPoints = input$colObservedPoints,
              i.colEpidemicStart = input$colEpidemicStart,
              i.colEpidemicStop = input$colEpidemicStop,
              i.colThresholds = input$colThresholds,
              i.colSeasons = input$colSeasons,
              i.colEpidemic = input$colEpidemic
            )

            p <- plotMAP(datfile.plot,
              i.textMain = input$textMain,
              i.textX = input$textX,
              i.textY = input$textY,
              i.method = as.numeric(input$method),
              i.param = as.numeric(input$param),
              i.colObservedLines = colors.palette$colObservedLines,
              i.colObservedPoints = colors.palette$colObservedPoints,
              i.colOptimum = colors.palette$colEpidemicStart,
              i.colLine = colors.palette$colEpidemic[3]
            )
            if (is.null(p)) {
              zfix <- NULL
            } else {
              z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
              zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
            }
          }
          zfix
        })
      })
      cat("observeEvent/data_model> updating timing graphs/slope curve... model\n")
      lapply(modseasons, function(s) {
        output[[paste0("tbmTiming_", as.character(s), "_slope")]] <- renderPlotly({
          readdata <- isolate(read_data())
          datfile <- readdata$datasetread
          if (is.null(datfile)) {
            zfix <- NULL
          } else if (!(as.character(s) %in% names(datfile))) {
            zfix <- NULL
          } else {
            datfile.plot <- datfile[as.character(s)]
            colors.palette <- generatePalette(
              i.number.series = NCOL(datfile.plot),
              i.colObservedLines = input$colObservedLines,
              i.colObservedPoints = input$colObservedPoints,
              i.colEpidemicStart = input$colEpidemicStart,
              i.colEpidemicStop = input$colEpidemicStop,
              i.colThresholds = input$colThresholds,
              i.colSeasons = input$colSeasons,
              i.colEpidemic = input$colEpidemic
            )
            p <- plotSlope(datfile.plot,
              i.textMain = input$textMain,
              i.textX = input$textX,
              i.textY = input$textY,
              i.method = as.numeric(input$method),
              i.param = as.numeric(input$param),
              i.colObservedLines = colors.palette$colObservedLines,
              i.colObservedPoints = colors.palette$colObservedPoints,
              i.colOptimum = colors.palette$colEpidemicStart,
              i.colLine1 = colors.palette$colEpidemic[2],
              i.colLine2 = colors.palette$colEpidemic[3]
            )
            if (is.null(p)) {
              zfix <- NULL
            } else {
              z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
              zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
            }
          }
          zfix
        })
      })
      cat("observeEvent/data_model> updating manual optimization plots... seasons' structure\n")
      lapply(modseasons, function(s) {
        output[[paste0("tbmOptimizeM_", as.character(s))]] <- renderUI({
          imgfileok <- FALSE
          if (NROW(values$optimizegraphs) > 0) {
            imgtmp <- values$optimizegraphs
            imgtmp2 <- subset(imgtmp, imgtmp$season == as.character(s))
            if (NROW(imgtmp2) > 0) {
              if (file.exists(imgtmp2$file)) {
                imgfile <- imgtmp2$file
                imgfileok <- TRUE
              }
            }
          }
          if (imgfileok) {
            fluidPage(
              fluidRow(
                column(1, h4(trloc("main.checkdescribe.timing"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, plotOutput(outputId = paste0("tbmOptimizeM_", as.character(s), "_plot"), click = paste0("tbmOptimizeM_", as.character(s), "_click"), width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1)))
              ),
              fluidRow(
                column(1, h4(trloc("main.model.optimize.manual.clicks"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, tableOutput(paste0("tbmOptimizeM_", as.character(s), "_table")))
              ),
              fluidRow(
                column(1, h4(trloc("main.model.optimize.manual.results"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, imageOutput(paste0("tbmOptimizeM_", as.character(s), "_image")))
              )
            )
          } else {
            fluidPage(
              fluidRow(
                column(1, h4(trloc("main.checkdescribe.timing"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, plotOutput(outputId = paste0("tbmOptimizeM_", as.character(s), "_plot"), click = paste0("tbmOptimizeM_", as.character(s), "_click"), width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1)))
              ),
              fluidRow(
                column(1, h4(trloc("main.model.optimize.manual.clicks"), tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}"))),
                column(11, tableOutput(paste0("tbmOptimizeM_", as.character(s), "_table")))
              )
            )
          }
        })
      })
      cat("observeEvent/data_model> updating manual optimization plots... start & end values table\n")
      lapply(modseasons, function(s) {
        output[[paste0("tbmOptimizeM_", as.character(s), "_table")]] <- renderTable({
          if (NROW(values$clickdata) > 0) {
            etwo <- extractTwo(values$clickdata, "weekno", "season")
            etwo <- merge(etwo, data.frame(id.tail = c(1, 2), point = c(trloc("graphs.start"), trloc("graphs.end")), stringsAsFactors = FALSE), by = "id.tail")
            etwo2 <- subset(etwo, etwo$season == as.character(s))[c("season", "weekno", "point", paste0(as.character(s), "_fixed"))]
            names(etwo2)[seq_len(3)] <- c(trloc("selections.surveillance.season.label"), trloc("graphs.week"), trloc("graphs.point"))
            names(etwo2)[4] <- as.character(s)
          } else {
            etwo2 <- data.frame(message = "No data")
          }
          etwo2
        })
      })
      cat("observeEvent/data_model> updating manual optimization plots... goodness image\n")
      lapply(modseasons, function(s) {
        output[[paste0("tbmOptimizeM_", as.character(s), "_image")]] <- renderImage(
          {
            imgfile <- ""
            if (NROW(values$optimizegraphs) > 0) {
              imgtmp <- values$optimizegraphs
              imgtmp2 <- subset(imgtmp, imgtmp$season == as.character(s))
              if (NROW(imgtmp2) > 0) {
                if (file.exists(imgtmp2$file)) {
                  imgfile <- imgtmp2$file
                }
              }
            }
            gfile <- list(src = imgfile, contentType = "image/png", width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1), alt = "No image found")
            gfile
          },
          deleteFile = FALSE
        )
      })
      cat("observeEvent/data_model> updating manual optimization plots... seasons plots\n")
      lapply(modseasons, function(s) {
        output[[paste0("tbmOptimizeM_", as.character(s), "_plot")]] <- renderPlot({
          if (as.character(s) %in% names(values$origdata)) {
            i.cutoff.original <- min(as.numeric(values$origdata$weekna[seq_len(min(3, NROW(values$origdata)))]))
            if (i.cutoff.original < 1) i.cutoff.original <- 1
            if (i.cutoff.original > 52) i.cutoff.original <- 52
            i.range.x <- c(min(as.numeric(values$origdata$weekna[seq_len(min(3, NROW(values$origdata)))])), max(as.numeric(values$origdata$weekna[(max(1, NROW(values$origdata) - 2)):NROW(values$origdata)])))
            if (i.range.x[1] < 1) i.range.x[1] <- 1
            if (i.range.x[1] > 52) i.range.x[1] <- 52
            if (i.range.x[2] < 1) i.range.x[2] <- 1
            if (i.range.x[2] > 52) i.range.x[2] <- 52
            if (i.range.x[1] == i.range.x[2]) i.range.x[2] <- i.range.x[2] - 1
            if (i.range.x[2] == 0) i.range.x[2] <- 52
            week.f <- i.range.x[1]
            week.l <- i.range.x[2]
            last.week <- 52
            if (week.f > week.l) {
              i.range.x.values <- data.frame(week.lab = c(week.f:last.week, seq_len(week.l)), week.no = seq_len(last.week - week.f + 1 + week.l))
            } else {
              i.range.x.values <- data.frame(week.lab = week.f:week.l, week.no = seq_len(week.l - week.f + 1))
            }
            # Calculate ticks for x
            data.x <- values$origdata$weekno
            axis.x.range <- range(data.x)
            temp1 <- range(i.range.x.values$week.no)
            temp2 <- mem:::optimal.tickmarks(temp1[1], temp1[2], 30, seq_len(temp1[2]), TRUE, TRUE)
            axis.x.ticks <- data.x[values$origdata$weekna %in% i.range.x.values$week.lab[temp2$tickmarks]]
            axis.x.labels <- values$origdata$weekna[values$origdata$weekna %in% i.range.x.values$week.lab[temp2$tickmarks]]
            rm("temp1", "temp2")
            # Range y fix
            i.range.y <- c(0, 1.05 * max(values$origdata[s], na.rm = TRUE))
            axis.y.range.original <- i.range.y
            axis.y.otick <- mem:::optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
            axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range)) * 0.025 * c(-1, 1)
            axis.y.ticks <- axis.y.otick$tickmarks
            axis.y.labels <- axis.y.otick$tickmarks
            colors.palette <- generatePalette(
              i.number.series = NCOL(values$plotdata),
              i.colObservedLines = input$colObservedLines,
              i.colObservedPoints = input$colObservedPoints,
              i.colEpidemicStart = input$colEpidemicStart,
              i.colEpidemicStop = input$colEpidemicStop,
              i.colThresholds = input$colThresholds,
              i.colSeasons = input$colSeasons,
              i.colEpidemic = input$colEpidemic
            )
            colormixed <- "#FF00FF"
            p1 <- ggplot(values$plotdata, aes_(x = as.name("weekno"), y = as.name(paste0(s, "_fixed")))) +
              geom_point(aes_(as.name("weekno"), as.name(s), colour = as.name(paste0(s, "_color")), size = as.name(paste0(s, "_color")))) +
              scale_color_manual(values = c("1" = colors.palette$colObservedPoints, "2" = colors.palette$colEpidemicStart, "3" = colors.palette$colEpidemicStop, "4" = colormixed)) +
              scale_size_manual(values = c("1" = 4, "2" = 6, "3" = 6, "4" = 8)) +
              geom_line(aes_(x = as.name("weekno"), y = as.name(s)), color = colors.palette$colObservedLines) +
              scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
              scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
              labs(title = input$textMain, x = input$textX, y = input$textY) +
              theme_light() +
              theme(plot.title = element_text(hjust = 0.5)) +
              guides(color = FALSE, size = FALSE)
          } else {
            p1 <- NULL
          }
          p1
        })
      })
      cat("observeEvent/data_model> updating manual optimization plots... click events\n")
      lapply(modseasons, function(s) {
        nameid <- paste0("tbmOptimizeM_", as.character(s), "_click")
        if (!(nameid %in% values$idscreated)) {
          values$idscreated <- c(values$idscreated, nameid)
          observeEvent(input[[nameid]], {
            np.x <- input[[nameid]]$mapping$x
            # Note: input[[nameid]] returns the point clicked, but when the original column has a complex name
            # it adds a ` at the begining and end, thus avoiding to detect the value from values$origdata, which
            # have normal names (without ``), so I have to change the yvar value
            np.y <- gsub("`", "", input[[nameid]]$mapping$y)
            np.max <- max(values$origdata[np.y], na.rm = TRUE) / 2
            np <- nearPoints(values$origdata, input[[nameid]],
              xvar = np.x,
              yvar = np.y,
              maxpoints = 1,
              threshold = np.max
            )
            if (NROW(np) > 0) values$clickdata <- rbind(values$clickdata, cbind(data.frame(season = as.character(s), stringsAsFactors = FALSE), np))
            if (NROW(values$clickdata) > 0) {
              p0 <- extractTwo(values$clickdata, "weekno", "season")
              p1 <- subset(p0, season == as.character(s) & id.tail == 1)
              p2 <- subset(p0, season == as.character(s) & id.tail == 2)
              if (NROW(p1) > 0) {
                values$plotdata[values$plotdata[, paste0(as.character(s), "_color")] != "3", paste0(as.character(s), "_color")] <- "1"
                values$plotdata[values$origdata$weekno == p1$weekno, paste0(as.character(s), "_color")] <- "2"
              }
              if (NROW(p2) > 0) {
                values$plotdata[values$plotdata[, paste0(as.character(s), "_color")] != "2", paste0(as.character(s), "_color")] <- "1"
                values$plotdata[values$origdata$weekno == p2$weekno, paste0(as.character(s), "_color")] <- "3"
              }
              if (NROW(p1) > 0 & NROW(p2) > 0) {
                if (p1$weekno == p2$weekno) {
                  values$plotdata[, paste0(as.character(s), "_color")] <- "1"
                  values$plotdata[values$origdata$weekno == p1$weekno, paste0(as.character(s), "_color")] <- "4"
                }
              }
            }
          })
        }
      })
    }
    cat("observeEvent/data_model> end\n")
  })

  observeEvent(input$resetuiTextoptions, {
    cat("observeEvent/resetuiTextoptions> begin\n")
    cat("observeEvent/resetuiTextoptions> reseting text options to default\n")
    updateTextInput(session, "textMain", value = trloc("options.text.main.label"))
    updateTextInput(session, "textY", value = trloc("options.text.yaxis.label"))
    updateTextInput(session, "textX", value = trloc("options.text.xaxis.label"))
    cat("observeEvent/resetuiTextoptions> end\n")
  })

  observeEvent(input$resetuiGraphoptions, {
    cat("observeEvent/resetuiGraphoptions> begin\n")
    cat("observeEvent/resetuiGraphoptionss> reseting graph options to default\n")
    updateSelectInput(session, "colObservedLines", selected = default_values$colObservedLines)
    updateSelectInput(session, "colObservedPoints", selected = default_values$colObservedPoints)
    updateSelectInput(session, "colEpidemicStart", selected = default_values$colEpidemicStart)
    updateSelectInput(session, "colEpidemicStop", selected = default_values$colEpidemicStop)
    updateSelectInput(session, "colThresholds", selected = default_values$colThresholds)
    updateSelectInput(session, "colLevels", selected = default_values$colLevels)
    updateSelectInput(session, "colSeasons", selected = default_values$colSeasons)
    updateSelectInput(session, "colEpidemic", selected = default_values$colEpidemic)
    updateCheckboxInput(session, "yaxis0", value = default_values$yaxis0)
    cat("observeEvent/resetuiGraphoptions> end\n")
  })

  observeEvent(input$resetuiMEMoptions, {
    cat("observeEvent/resetuiMEMoptions> begin\n")
    cat("observeEvent/resetuiMEMoptions> reseting MEM options to default\n")
    updateSelectInput(session, "method", selected = default_values$method)
    updateNumericInput(session, "param", value = default_values$param$value, min = default_values$param$min, max = default_values$param$max, step = default_values$param$step)
    updateNumericInput(session, "optimparam", value = default_values$optimparam$value, min = default_values$optimparam$min, max = default_values$optimparam$max, step = default_values$optimparam$step)
    updateSelectInput(session, "nvalues", selected = default_values$nvalues)
    updateNumericInput(session, "ntails", value = default_values$ntails$value, min = default_values$ntails$min, max = default_values$ntails$max, step = default_values$ntails$step)
    updateSelectInput(session, "typethreshold", selected = default_values$typethreshold)
    updateSelectInput(session, "typeintensity", selected = default_values$typeintensity)
    updateNumericInput(session, "levelintensitym", value = default_values$levelintensitym$value, min = default_values$levelintensitym$min, max = default_values$levelintensitym$max, step = default_values$levelintensitym$step)
    updateNumericInput(session, "levelintensityh", value = default_values$levelintensityh$value, min = default_values$levelintensityh$min, max = default_values$levelintensityh$max, step = default_values$levelintensityh$step)
    updateNumericInput(session, "levelintensityv", value = default_values$levelintensityv$value, min = default_values$levelintensityv$min, max = default_values$levelintensityv$max, step = default_values$levelintensityv$step)
    updateSelectInput(session, "validation", selected = default_values$validation)
    updateSelectInput(session, "optimmethod", selected = default_values$optimmethod)
    updateSliderInput(session, "paramrange", value = default_values$paramrange$value, min = default_values$paramrange$min, max = default_values$paramrange$max, step = default_values$paramrange$step)
    updateNumericInput(session, "minsensitivity", value = default_values$minsensitivity$value, min = default_values$minsensitivity$min, max = default_values$minsensitivity$max, step = default_values$minsensitivity$step)
    updateNumericInput(session, "minspecificity", value = default_values$minspecificity$value, min = default_values$minspecificity$min, max = default_values$minspecificity$max, step = default_values$minspecificity$step)
    updateSelectInput(session, "typeaveragecurve", selected = default_values$typeaveragecurve)
    updateSelectInput(session, "typeother", selected = default_values$typeother)
    updateNumericInput(session, "levelaveragecurve", value = default_values$levelaveragecurve$value, min = default_values$levelaveragecurve$min, max = default_values$levelaveragecurve$max, step = default_values$levelaveragecurve$step)
    updateNumericInput(session, "levelother", value = default_values$levelother$value, min = default_values$levelother$min, max = default_values$levelother$max, step = default_values$levelother$step)
    updateSelectInput(session, "centering", selected = default_values$centering)
    updateMaterialSwitch(session, "usetdistribution", value = default_values$usetdistribution)
    cat("observeEvent/resetuiMEMoptions> end\n")
  })

  observeEvent(input$makeLangDefault, {
    cat("observeEvent/makeLangDefault> begin\n")
    cat("observeEvent/makeLangDefault> Setting new default language\n")
    setLanguage(input$language)
    cat("observeEvent/makeLangDefault> end\n")
  })

  observeEvent(input$applyOptimParam, {
    cat("observeEvent/applyOptimParam> begin\n")
    cat("observeEvent/applyOptimParam> Setting optimum parameter\n")
    updateNumericInput(session, "param", value = input$optimparam)
    cat("observeEvent/applyOptimParam> end\n")
  })

  observeEvent(input$dimension, {
    cat("observeEvent/dimension> begin\n")
    # Formato 4x3
    gfactor <- 0.65
    gwidth1 <- round(gfactor * input$dimension[1], digits = 0)
    gwidth2 <- gwidth1
    gheight1 <- round(gfactor * input$dimension[1] / 4 * 3, digits = 0)
    # Formato 16x9
    gheight2 <- round(gfactor * input$dimension[1] / 16 * 9, digits = 0)
    # Max height 400
    mheight1 <- min(400, gheight1)
    mwidth1 <- round(mheight1 / 3 * 4, digits = 0)
    mheight2 <- min(400, gheight2)
    mwidth2 <- round(mheight2 / 9 * 16, digits = 0)
    cat("observeEvent/dimension> updating dimensions\t", gwidth1, "x", gheight1, "\t", gwidth2, "x", gheight2, "\t", mwidth1, "x", mheight1, "\t", mwidth2, "x", mheight2, "\n")
    values$gwidth1 <- gwidth1
    values$gheight1 <- gheight1
    values$gwidth2 <- gwidth2
    values$gheight2 <- gheight2
    values$mwidth1 <- mwidth1
    values$mheight1 <- mheight1
    values$mwidth2 <- mwidth2
    values$mheight2 <- mheight2
    cat("observeEvent/dimension> end\n")
  })

  #####################################
  ### DEFINING TABS STRUCTURE
  #####################################

  #####################################
  ### DATA TAB
  #####################################

  output$tbData <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      tabsetPanel(tabPanel(trloc("main.checkdescribe.file"), uiOutput("tbdFile")))
    } else {
      if (as.logical(input$advanced)) {
        tabsetPanel(
          tabPanel(trloc("main.checkdescribe.file"), uiOutput("tbdFile")),
          tabPanel(
            trloc("main.checkdescribe.data"),
            DT::dataTableOutput("tbdData"),
            fluidRow(
              column(8),
              column(
                2,
                if (zipPresent() & openxlsxPresent()) {
                  downloadButton("tbdData_x", "xlsx")
                } else if (!openxlsxPresent()) {
                  shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
                } else if (.Platform$OS.type == "windows") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                } else if (.Platform$OS.type == "unix") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
                }
              ),
              column(2, downloadButton("tbdData_c", "csv"))
            )
          ),
          tabPanel(trloc("main.checkdescribe.seasons"), plotlyOutput("tbdSeasons", width = "auto", height = "auto")),
          tabPanel(trloc("main.checkdescribe.series"), plotlyOutput("tbdSeries", width = "auto", height = "auto")),
          tabPanel(trloc("main.checkdescribe.timing"), uiOutput("tbdTiming")),
          tabPanel(trloc("main.checkdescribe.evolution"), uiOutput("tbdEvolution")),
          tabPanel(trloc("main.checkdescribe.stability"), uiOutput("tbdStability")),
          tabPanel(trloc("main.checkdescribe.goodness"), uiOutput("tbdGoodness"))
        )
      } else {
        tabsetPanel(
          tabPanel(trloc("main.checkdescribe.file"), uiOutput("tbdFile")),
          tabPanel(
            trloc("main.checkdescribe.data"),
            DT::dataTableOutput("tbdData"),
            fluidRow(
              column(8),
              column(
                2,
                if (zipPresent() & openxlsxPresent()) {
                  downloadButton("tbdData_x", "xlsx")
                } else if (!openxlsxPresent()) {
                  shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
                } else if (.Platform$OS.type == "windows") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                } else if (.Platform$OS.type == "unix") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
                }
              ),
              column(2, downloadButton("tbdData_c", "csv"))
            )
          ),
          tabPanel(trloc("main.checkdescribe.seasons"), plotlyOutput("tbdSeasons", width = "auto", height = "auto")),
          tabPanel(trloc("main.checkdescribe.series"), plotlyOutput("tbdSeries", width = "auto", height = "auto")),
          tabPanel(trloc("main.checkdescribe.timing"), uiOutput("tbdTiming")),
          tabPanel(trloc("main.checkdescribe.evolution"), uiOutput("tbdEvolution")),
          tabPanel(trloc("main.checkdescribe.stability"), uiOutput("tbdStability"))
        )
      }
    }
  })

  output$tbdFile <- renderUI({
    if (as.numeric(input$waves) == 4) {
      fluidPage(
        fluidRow(
          column(
            12, h5(
              trloc("selections.dataset.wavesdetection.multiple.summary"),
              tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}")
            ),
            verbatimTextOutput("tbdFileTxt")
          )
        ),
        fluidRow(
          column(
            12, h5(
              trloc("selections.dataset.wavesdetection.multiple"),
              tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}")
            ),
            plotlyOutput("tbdFilePlot1", width = "auto", height = "auto")
          )
        ),
        fluidRow(
          column(
            6, h5(
              trloc("selections.dataset.wavesdetection.multiple.epidemics"),
              tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}")
            ),
            plotlyOutput("tbdFilePlot2", width = "auto", height = "auto")
          ),
          column(
            6, h5(
              trloc("selections.dataset.wavesdetection.multiple.location"),
              tags$style(type = "text/css", "#q1 {font-weight: bold;float:right;}")
            ),
            plotlyOutput("tbdFilePlot3", width = "auto", height = "auto")
          )
        )
      )
    } else {
      verbatimTextOutput("tbdFileTxt")
    }
  })

  output$tbdFileTxt <- renderPrint({
    infile <- input$file
    indataset <- input$dataset
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      cat(trloc("ui.nofile"), "\n", sep = "")
      cat(trloc("ui.log"), ":\n\t", sep = "")
      cat(gsub("\n", "\n\t", readdata$datalog, fixed = TRUE), sep = "")
    } else {
      cat(trloc("main.checkdescribe.file"), ":\n\t", infile$name, "\n", sep = "")
      cat(trloc("selections.dataset"), ":\n\t", indataset, "\n", sep = "")
      cat(trloc("ui.log"), ":\n\t", sep = "")
      cat(gsub("\n", "\n\t", readdata$datalog, fixed = TRUE), sep = "")
    }
  })

  output$tbdFilePlot1 <- renderPlotly({
    readdata <- read_data()
    plots <- readdata$plots
    if (is.null(plots)) {
      zfix <- NULL
    } else {
      zfix <- ggplotly(plots$p3[[length(plots$p3)]], width = as.numeric(values$gwidth2), height = as.numeric(values$gheight2))
    }
    zfix
  })

  output$tbdFilePlot2 <- renderPlotly({
    readdata <- read_data()
    plots <- readdata$plots
    if (is.null(plots)) {
      zfix <- NULL
    } else {
      zfix <- ggplotly(plots$p4[[2]], width = as.numeric(values$gwidth1) / 2, height = as.numeric(values$gheight1) / 2)
    }
    zfix
  })

  output$tbdFilePlot3 <- renderPlotly({
    readdata <- read_data()
    plots <- readdata$plots
    if (is.null(plots)) {
      zfix <- NULL
    } else {
      zfix <- ggplotly(plots$p5[[2]], width = as.numeric(values$gwidth1) / 2, height = as.numeric(values$gheight1) / 2)
    }
    zfix
  })

  output$tbdData <- DT::renderDataTable(
    {
      readdata <- read_data()
      datfile <- readdata$datasetread
      if (is.null(datfile)) {
        datatoshow <- NULL
      } else {
        # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
        selectedcolumns <- selectColumns(
          i.names = names(datfile),
          i.from = "",
          i.to = "",
          i.exclude = "",
          i.include = "",
          i.pandemic = TRUE,
          i.seasons = NA
        )
        if (length(selectedcolumns) > 0) {
          datatoshow <- format(round(datfile[selectedcolumns], 2), nsmall = 2)
        } else {
          datatoshow <- data.frame(Message = "No data selected", row.names = NULL)
        }
      }
      datatoshow
    },
    options = list(scrollX = TRUE, scrollY = "600px", paging = FALSE, dom = "Bfrtip", columnDefs = list(list(targets = "_all", class = "dt-right")))
  )

  output$tbdData_x <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      readdata <- read_data()
      datfile <- readdata$datasetread
      if (!is.null(datfile)) {
        selectedcolumns <- selectColumns(i.names = names(datfile), i.from = "", i.to = "", i.exclude = "", i.include = "", i.pandemic = TRUE, i.seasons = NA)
        if (length(selectedcolumns) > 0) {
          exportData(
            i.data = datfile[selectedcolumns], i.file = file,
            i.sheet = substring(trloc("main.checkdescribe.data"), 1, 32), i.rownames = trloc("app.weekno"), i.format = "xlsx"
          )
        }
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$tbdData_c <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      readdata <- read_data()
      datfile <- readdata$datasetread
      if (!is.null(datfile)) {
        selectedcolumns <- selectColumns(i.names = names(datfile), i.from = "", i.to = "", i.exclude = "", i.include = "", i.pandemic = TRUE, i.seasons = NA)
        if (length(selectedcolumns) > 0) {
          exportData(
            i.data = datfile[selectedcolumns], i.file = file,
            i.sheet = substring(trloc("main.checkdescribe.data"), 1, 32), i.rownames = trloc("app.weekno"), i.format = "csv"
          )
        }
      }
    },
    contentType = "text/csv"
  )

  output$tbdSeasons <- renderPlotly({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      zfix <- NULL
    } else {
      datamodel <- data_model()
      if (is.null(datamodel)) {
        zfix <- NULL
      } else {
        datfile.plot <- datfile
        e.thr <- datamodel$epidemic.thresholds
        i.thr <- datamodel$intensity.thresholds
        colors.palette <- generatePalette(
          i.number.series = NCOL(datfile.plot),
          i.colObservedLines = input$colObservedLines,
          i.colObservedPoints = input$colObservedPoints,
          i.colEpidemicStart = input$colEpidemicStart,
          i.colEpidemicStop = input$colEpidemicStop,
          i.colThresholds = input$colThresholds,
          i.colSeasons = input$colSeasons,
          i.colEpidemic = input$colEpidemic
        )
        p <- plotSeasons(datfile.plot,
          i.epidemic.thr = e.thr,
          i.intensity.thr = i.thr,
          i.pre.epidemic = as.logical(input$preepidemicthr),
          i.post.epidemic = as.logical(input$postepidemicthr),
          i.intensity = as.logical(input$intensitythr),
          i.textMain = input$textMain,
          i.textX = input$textX,
          i.textY = input$textY,
          i.type.threshold = as.numeric(input$typethreshold),
          i.tails.threshold = as.numeric(input$ntails),
          i.type.intensity = as.numeric(input$typeintensity),
          i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
          i.tails.intensity = as.numeric(input$ntails),
          i.type.curve = as.numeric(input$typeaveragecurve),
          i.level.curve = as.numeric(input$levelaveragecurve) / 100,
          i.type.other = as.numeric(input$typeother),
          i.level.other = as.numeric(input$levelother) / 100,
          i.method = as.numeric(input$method),
          i.param = as.numeric(input$param),
          i.centering = as.numeric(input$centering),
          i.n.max = as.numeric(input$nvalues),
          i.colObservedPoints = colors.palette$colObservedPoints,
          i.colSeasons = colors.palette$colSeasons,
          i.colThresholds = colors.palette$colThresholds,
          i.yaxis.starts.at.0 = as.logical(input$yaxis0),
          i.use.t = as.logical(input$usetdistribution)
        )
        if (is.null(p)) {
          zfix <- NULL
        } else {
          z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
          zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
        }
      }
    }
    zfix
  })

  output$tbdSeries <- renderPlotly({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      zfix <- NULL
    } else {
      datamodel <- data_model()
      if (is.null(datamodel)) {
        zfix <- NULL
      } else {
        datfile.plot <- datfile
        e.thr <- datamodel$epidemic.thresholds
        i.thr <- datamodel$intensity.thresholds
        colors.palette <- generatePalette(
          i.number.series = NA,
          i.colObservedLines = input$colObservedLines,
          i.colObservedPoints = input$colObservedPoints,
          i.colEpidemicStart = input$colEpidemicStart,
          i.colEpidemicStop = input$colEpidemicStop,
          i.colThresholds = input$colThresholds,
          i.colSeasons = input$colSeasons,
          i.colEpidemic = input$colEpidemic
        )
        p <- plotSeries(
          i.data = datfile.plot,
          i.plot.timing = as.logical(input$plottiming),
          i.range.x = NA,
          i.pre.epidemic = as.logical(input$preepidemicthr),
          i.post.epidemic = as.logical(input$postepidemicthr),
          i.epidemic.thr = e.thr,
          i.intensity = as.logical(input$intensitythr),
          i.intensity.thr = i.thr,
          i.range.y = NA,
          i.replace.x.cr = TRUE,
          i.textMain = input$textMain,
          i.textX = input$textX,
          i.textY = input$textY,
          i.type.threshold = as.numeric(input$typethreshold),
          i.tails.threshold = as.numeric(input$ntails),
          i.type.intensity = as.numeric(input$typeintensity),
          i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
          i.tails.intensity = as.numeric(input$ntails),
          i.type.curve = as.numeric(input$typeaveragecurve),
          i.level.curve = as.numeric(input$levelaveragecurve) / 100,
          i.type.other = as.numeric(input$typeother),
          i.level.other = as.numeric(input$levelother) / 100,
          i.method = as.numeric(input$method),
          i.param = as.numeric(input$param),
          i.centering = as.numeric(input$centering),
          i.n.max = as.numeric(input$nvalues),
          i.colObservedLines = colors.palette$colObservedLines,
          i.colThresholds = colors.palette$colThresholds,
          i.colObservedPoints = colors.palette$colObservedPoints,
          i.colEpidemic = colors.palette$colEpidemic,
          i.yaxis.starts.at.0 = as.logical(input$yaxis0),
          i.use.t = as.logical(input$usetdistribution)
        )
        if (is.null(p)) {
          zfix <- NULL
        } else {
          z <- ggplotly(p$plot, width = as.numeric(values$gwidth2), height = as.numeric(values$gheight2))
          zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
        }
      }
    }
    zfix
  })

  output$tbdTiming <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      return(NULL)
    } else {
      selectedcolumns <- selectColumns(
        i.names = names(datfile),
        i.from = "",
        i.to = "",
        i.exclude = "",
        i.include = "",
        i.pandemic = TRUE,
        i.seasons = NA
      )
      datfile.plot <- datfile[selectedcolumns]
      tabnames <- names(datfile.plot)
      do.call(
        tabsetPanel,
        ## Create a set of tabPanel functions dependent on tabnames
        lapply(tabnames, function(s) {
          ## Populate the tabPanel with a dataTableOutput layout, with ID specific to the sample.
          ## Can also accommodate additional layout parts by adding additional call() to call("tabPanel")
          call("tabPanel", s, call("uiOutput", outputId = paste0("tbdTiming_", s), width = "auto", height = "auto"))
        })
      )
    }
  })

  output$tbdEvolution <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      return(NULL)
    } else {
      tabsetPanel(
        tabPanel(trloc("main.checkdescribe.evolution.duration"), plotlyOutput("tbdEduration", width = "auto", height = "auto")),
        tabPanel(trloc("main.checkdescribe.evolution.start"), plotlyOutput("tbdEstart", width = "auto", height = "auto")),
        tabPanel(trloc("main.checkdescribe.evolution.epidemicperc"), plotlyOutput("tbdEpercentage", width = "auto", height = "auto")),
        tabPanel(trloc("main.checkdescribe.evolution.thresholds"), plotlyOutput("tbdEthresholds", width = "auto", height = "auto")),
        tabPanel(trloc("main.checkdescribe.evolution.scheme"), formattable::formattableOutput("tbdEscheme")),
        tabPanel(
          trloc("main.checkdescribe.evolution.detailed"),
          DT::dataTableOutput("tbdEdetailed"),
          fluidRow(
            column(8),
            column(
              2,
              if (zipPresent() & openxlsxPresent()) {
                downloadButton("tbdEdetailed_x", "xlsx")
              } else if (!openxlsxPresent()) {
                shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
              } else if (.Platform$OS.type == "windows") {
                shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
              } else if (.Platform$OS.type == "unix") {
                shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
              }
            ),
            column(2, downloadButton("tbdEdetailed_c", "csv"))
          )
        )
      )
    }
  })


  output$tbdEduration <- renderPlotly({
    dataevolution <- data_evolution()$evolution.data
    if (is.null(dataevolution)) {
      zfix <- NULL
    } else {
      indicators <- c("durationll", "duration", "durationul")
      datfile.plot <- dataevolution[indicators]
      names(datfile.plot) <- c(trloc("graphs.durationlower"), trloc("graphs.duration"), trloc("graphs.durationupper"))
      # by inserting \n instead of /, the fixPlotly function assign twice the space for the x-axis labs
      colors.palette <- generatePalette(
        i.number.series = 3,
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      p <- plotGeneric(datfile.plot,
        i.range.y = NA,
        i.range.y.labels = NA,
        i.shapes = rep(21, NCOL(datfile.plot)),
        i.colors = colors.palette$colSeasons,
        i.fills = colors.palette$colSeasons,
        i.sizes = rep(3, NCOL(datfile.plot)),
        i.linetypes = rep("solid", NCOL(datfile.plot)),
        i.linesize = 1,
        i.replace.x.cr = TRUE,
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0)
      )
      if (is.null(p)) {
        zfix <- NULL
      } else {
        z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
        zfix <- fixPlotly(
          z,
          names(datfile.plot),
          rep(TRUE, NCOL(datfile.plot)),
          rep(TRUE, NCOL(datfile.plot)),
          trloc("selections.surveillance.season.label"),
          "value",
          rownames(datfile.plot)
        )
      }
    }
    zfix
  })

  output$tbdEstart <- renderPlotly({
    dataevolution <- data_evolution()$evolution.data
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(dataevolution)) {
      zfix <- NULL
    } else {
      indicators <- c("startll", "start", "startul")
      datfile.plot <- dataevolution[indicators]
      names(datfile.plot) <- c(trloc("graphs.startlower"), trloc("graphs.start"), trloc("graphs.startupper"))
      # by inserting \n instead of /, the fixPlotly function assign twice the space for the x-axis labs
      colors.palette <- generatePalette(
        i.number.series = 3,
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      p <- plotGeneric(datfile.plot,
        i.range.y = NA,
        i.range.y.labels = rownames(datfile),
        i.shapes = rep(21, NCOL(datfile.plot)),
        i.colors = colors.palette$colSeasons,
        i.fills = colors.palette$colSeasons,
        i.sizes = rep(3, NCOL(datfile.plot)),
        i.linetypes = rep("solid", NCOL(datfile.plot)),
        i.linesize = 1,
        i.replace.x.cr = TRUE,
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0)
      )

      if (is.null(p)) {
        zfix <- NULL
      } else {
        z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
        zfix <- fixPlotly(
          z,
          names(datfile.plot),
          rep(TRUE, NCOL(datfile.plot)),
          rep(TRUE, NCOL(datfile.plot)),
          trloc("selections.surveillance.season.label"),
          "value",
          rownames(datfile.plot)
        )
        # fix to replace relative to absolute weeks
        for (i in seq_len(3)) zfix$x$data[[i]]$text <- paste(trloc("selections.surveillance.season.label"), ": ", rownames(datfile.plot), "<br />", names(datfile.plot)[i], ": ", rownames(datfile)[datfile.plot[, i]], sep = "")
      }
    }
    zfix
  })

  output$tbdEpercentage <- renderPlotly({
    dataevolution <- data_evolution()$evolution.data
    if (is.null(dataevolution)) {
      zfix <- NULL
    } else {
      indicators <- c("percentagell", "percentage", "percentageul")
      datfile.plot <- dataevolution[indicators]
      names(datfile.plot) <- c(trloc("graphs.epidemicpercentagelower"), trloc("graphs.epidemicpercentage"), trloc("graphs.epidemicpercentageupper"))
      # by inserting \n instead of /, the fixPlotly function assign twice the space for the x-axis labs
      colors.palette <- generatePalette(
        i.number.series = 3,
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      p <- plotGeneric(datfile.plot,
        i.range.y = NA,
        i.range.y.labels = NA,
        i.shapes = rep(21, NCOL(datfile.plot)),
        i.colors = colors.palette$colSeasons,
        i.fills = colors.palette$colSeasons,
        i.sizes = rep(3, NCOL(datfile.plot)),
        i.linetypes = rep("solid", NCOL(datfile.plot)),
        i.linesize = 1,
        i.replace.x.cr = TRUE,
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0)
      )
      if (is.null(p)) {
        zfix <- NULL
      } else {
        z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
        zfix <- fixPlotly(
          z,
          names(datfile.plot),
          rep(TRUE, NCOL(datfile.plot)),
          rep(TRUE, NCOL(datfile.plot)),
          trloc("selections.surveillance.season.label"),
          "value",
          rownames(datfile.plot)
        )
      }
    }
    zfix
  })

  output$tbdEthresholds <- renderPlotly({
    dataevolution <- data_evolution()$evolution.data
    if (is.null(dataevolution)) {
      zfix <- NULL
    } else {
      indicators <- c("epidemic", "medium", "high", "veryhigh", "postepidemic")
      datfile.plot <- dataevolution[indicators]
      names(datfile.plot) <- c(trloc("graphs.prethreshold"), trloc("graphs.mediumthreshold.short"), trloc("graphs.highthreshold.short"), trloc("graphs.veryhighthreshold.short"), trloc("graphs.postthreshold"))
      colors.palette <- generatePalette(
        i.number.series = NCOL(datfile.plot),
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      # by inserting \n instead of /, the fixPlotly function assign twice the space for the x-axis labs
      p <- plotGeneric(datfile.plot,
        i.range.y = NA,
        i.range.y.labels = NA,
        i.shapes = rep(21, NCOL(datfile.plot)),
        i.colors = colors.palette$colThresholds,
        i.fills = colors.palette$colThresholds,
        i.sizes = rep(3, NCOL(datfile.plot)),
        i.linetypes = rep("solid", NCOL(datfile.plot)),
        i.linesize = 1,
        i.replace.x.cr = TRUE,
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0)
      )
      if (is.null(p)) {
        zfix <- NULL
      } else {
        z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
        zfix <- fixPlotly(
          z,
          names(datfile.plot),
          rep(TRUE, NCOL(datfile.plot)),
          rep(TRUE, NCOL(datfile.plot)),
          trloc("selections.surveillance.season.label"),
          "value",
          rownames(datfile.plot)
        )
      }
    }
    zfix
  })

  output$tbdEscheme <- formattable::renderFormattable({
    dataevolution <- data_evolution()
    if (is.null(dataevolution)) {
      datashow <- NULL
    } else {
      temp1 <- dataevolution$evolution.seasons
      if (row.names(temp1)[NROW(temp1)] == "next") row.names(temp1)[NROW(temp1)] <- trloc("graphs.next")
      datashow <- formattable::formattable(temp1, apply(
        temp1, 2,
        function(noxneeded) {
          formattable::formatter("span",
            style = x ~ formattable::style(color = ifelse(x, "green", "red")),
            x ~ formattable::icontext(ifelse(x, "ok", "remove"), ifelse(x, trloc("graphs.yes"), trloc("graphs.no")))
          )
        }
      ))
    }
    datashow
  })

  output$tbdEdetailed <- DT::renderDataTable(
    {
      dataevolution <- data_evolution()
      if (is.null(dataevolution)) {
        datashow <- NULL
      } else {
        datashow <- format(round(dataevolution$evolution.data, 2), nsmall = 2)
        if (row.names(datashow)[NROW(datashow)] == "next") row.names(datashow)[NROW(datashow)] <- trloc("graphs.next")
        names(datashow) <- c(trloc("main.checkdescribe.seasons"), trloc("graphs.durationlower"), trloc("graphs.duration"), trloc("graphs.durationupper"), trloc("graphs.startlower"), trloc("graphs.start"), trloc("graphs.startupper"), trloc("graphs.epidemicpercentagelower"), trloc("graphs.epidemicpercentage"), trloc("graphs.epidemicpercentageupper"), trloc("graphs.prethreshold.short"), trloc("graphs.postthreshold"), trloc("graphs.mediumthreshold.short"), trloc("graphs.highthreshold.short"), trloc("graphs.veryhighthreshold.short"))
      }
      datashow
    },
    options = list(scrollX = TRUE, scrollY = "600px", paging = FALSE, dom = "Bfrtip", columnDefs = list(list(targets = "_all", class = "dt-right")))
  )

  output$tbdEdetailed_x <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      dataevolution <- data_evolution()
      datashow <- dataevolution$evolution.data
      if (row.names(datashow)[NROW(datashow)] == "next") row.names(datashow)[NROW(datashow)] <- trloc("graphs.next")
      names(datashow) <- c(trloc("main.checkdescribe.seasons"), trloc("graphs.durationlower"), trloc("graphs.duration"), trloc("graphs.durationupper"), trloc("graphs.startlower"), trloc("graphs.start"), trloc("graphs.startupper"), trloc("graphs.epidemicpercentagelower"), trloc("graphs.epidemicpercentage"), trloc("graphs.epidemicpercentageupper"), trloc("graphs.prethreshold.short"), trloc("graphs.postthreshold"), trloc("graphs.mediumthreshold.short"), trloc("graphs.highthreshold.short"), trloc("graphs.veryhighthreshold.short"))
      if (!is.null(dataevolution)) {
        exportData(
          i.data = datashow, i.file = file,
          i.sheet = substring(trloc("main.checkdescribe.evolution"), 1, 32), i.rownames = trloc("selections.surveillance.season.label"), i.format = "xlsx"
        )
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$tbdEdetailed_c <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      dataevolution <- data_evolution()
      datashow <- dataevolution$evolution.data
      if (row.names(datashow)[NROW(datashow)] == "next") row.names(datashow)[NROW(datashow)] <- trloc("graphs.next")
      names(datashow) <- c(trloc("main.checkdescribe.seasons"), trloc("graphs.durationlower"), trloc("graphs.duration"), trloc("graphs.durationupper"), trloc("graphs.startlower"), trloc("graphs.start"), trloc("graphs.startupper"), trloc("graphs.epidemicpercentagelower"), trloc("graphs.epidemicpercentage"), trloc("graphs.epidemicpercentageupper"), trloc("graphs.prethreshold.short"), trloc("graphs.postthreshold"), trloc("graphs.mediumthreshold.short"), trloc("graphs.highthreshold.short"), trloc("graphs.veryhighthreshold.short"))
      if (!is.null(dataevolution)) {
        exportData(
          i.data = datashow, i.file = file,
          i.sheet = substring(trloc("main.checkdescribe.evolution"), 1, 32), i.rownames = trloc("selections.surveillance.season.label"), i.format = "csv"
        )
      }
    },
    contentType = "text/csv"
  )

  output$tbdStability <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      return(NULL)
    } else {
      tabsetPanel(
        tabPanel(trloc("main.checkdescribe.evolution.duration"), plotlyOutput("tbdSduration", width = "auto", height = "auto")),
        tabPanel(trloc("main.checkdescribe.evolution.start"), plotlyOutput("tbdSstart", width = "auto", height = "auto")),
        tabPanel(trloc("main.checkdescribe.evolution.epidemicperc"), plotlyOutput("tbdSpercentage", width = "auto", height = "auto")),
        tabPanel(trloc("main.checkdescribe.evolution.thresholds"), plotlyOutput("tbdSthresholds", width = "auto", height = "auto")),
        tabPanel(trloc("main.checkdescribe.evolution.scheme"), formattable::formattableOutput("tbdSscheme")),
        tabPanel(
          trloc("main.checkdescribe.evolution.detailed"),
          DT::dataTableOutput("tbdSdetailed"),
          fluidRow(
            column(8),
            column(
              2,
              if (zipPresent() & openxlsxPresent()) {
                downloadButton("tbdSdetailed_x", "xlsx")
              } else if (!openxlsxPresent()) {
                shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
              } else if (.Platform$OS.type == "windows") {
                shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
              } else if (.Platform$OS.type == "unix") {
                shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
              }
            ),
            column(2, downloadButton("tbdSdetailed_c", "csv"))
          )
        )
      )
    }
  })

  output$tbdSduration <- renderPlotly({
    datastability <- data_stability()$stability.data
    if (is.null(datastability)) {
      zfix <- NULL
    } else {
      indicators <- c("durationll", "duration", "durationul")
      datfile.plot <- datastability[indicators]
      names(datfile.plot) <- c(trloc("graphs.durationlower"), trloc("graphs.duration"), trloc("graphs.durationupper"))
      colors.palette <- generatePalette(
        i.number.series = 3,
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      p <- plotGeneric(datfile.plot,
        i.range.y = NA,
        i.range.y.labels = NA,
        i.shapes = rep(21, NCOL(datfile.plot)),
        i.colors = colors.palette$colSeasons,
        i.fills = colors.palette$colSeasons,
        i.sizes = rep(3, NCOL(datfile.plot)),
        i.linetypes = rep("solid", NCOL(datfile.plot)),
        i.linesize = 1,
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0)
      )
      if (is.null(p)) {
        zfix <- NULL
      } else {
        z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
        zfix <- fixPlotly(
          z,
          names(datfile.plot),
          rep(TRUE, NCOL(datfile.plot)),
          rep(TRUE, NCOL(datfile.plot)),
          trloc("main.checkdescribe.seasons"),
          "value",
          rownames(datfile.plot)
        )
      }
    }
    zfix
  })

  output$tbdSstart <- renderPlotly({
    datastability <- data_stability()$stability.data
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datastability)) {
      zfix <- NULL
    } else {
      indicators <- c("startll", "start", "startul")
      datfile.plot <- datastability[indicators]
      names(datfile.plot) <- c(trloc("graphs.startlower"), trloc("graphs.start"), trloc("graphs.startupper"))
      colors.palette <- generatePalette(
        i.number.series = 3,
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      p <- plotGeneric(datfile.plot,
        i.range.y = NA,
        i.shapes = rep(21, NCOL(datfile.plot)),
        i.range.y.labels = rownames(datfile),
        i.colors = colors.palette$colSeasons,
        i.fills = colors.palette$colSeasons,
        i.sizes = rep(3, NCOL(datfile.plot)),
        i.linetypes = rep("solid", NCOL(datfile.plot)),
        i.linesize = 1,
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0)
      )
      if (is.null(p)) {
        zfix <- NULL
      } else {
        z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
        zfix <- fixPlotly(
          z,
          names(datfile.plot),
          rep(TRUE, NCOL(datfile.plot)),
          rep(TRUE, NCOL(datfile.plot)),
          trloc("main.checkdescribe.seasons"),
          "value",
          rownames(datfile.plot)
        )
        # fix to replace relative to absolute weeks
        for (i in seq_len(3)) zfix$x$data[[i]]$text <- paste(trloc("main.checkdescribe.seasons"), ": ", rownames(datfile.plot), "<br />", names(datfile.plot)[i], ": ", rownames(datfile)[datfile.plot[, i]], sep = "")
      }
    }
    zfix
  })

  output$tbdSpercentage <- renderPlotly({
    datastability <- data_stability()$stability.data
    if (is.null(datastability)) {
      zfix <- NULL
    } else {
      indicators <- c("percentagell", "percentage", "percentageul")
      datfile.plot <- datastability[indicators]
      names(datfile.plot) <- c(trloc("graphs.epidemicpercentagelower"), trloc("graphs.epidemicpercentage"), trloc("graphs.epidemicpercentageupper"))
      colors.palette <- generatePalette(
        i.number.series = 3,
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      p <- plotGeneric(datfile.plot,
        i.range.y = NA,
        i.range.y.labels = NA,
        i.shapes = rep(21, NCOL(datfile.plot)),
        i.colors = colors.palette$colSeasons,
        i.fills = colors.palette$colSeasons,
        i.sizes = rep(3, NCOL(datfile.plot)),
        i.linetypes = rep("solid", NCOL(datfile.plot)),
        i.linesize = 1,
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0)
      )
      if (is.null(p)) {
        zfix <- NULL
      } else {
        z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
        zfix <- fixPlotly(
          z,
          names(datfile.plot),
          rep(TRUE, NCOL(datfile.plot)),
          rep(TRUE, NCOL(datfile.plot)),
          trloc("main.checkdescribe.seasons"),
          "value",
          rownames(datfile.plot)
        )
      }
    }
    zfix
  })

  output$tbdSthresholds <- renderPlotly({
    datastability <- data_stability()$stability.data
    if (is.null(datastability)) {
      zfix <- NULL
    } else {
      indicators <- c("epidemic", "medium", "high", "veryhigh", "postepidemic")
      datfile.plot <- datastability[indicators]
      names(datfile.plot) <- c(trloc("graphs.prethreshold"), trloc("graphs.mediumthreshold.short"), trloc("graphs.highthreshold.short"), trloc("graphs.veryhighthreshold.short"), trloc("graphs.postthreshold"))
      colors.palette <- generatePalette(
        i.number.series = NCOL(datfile.plot),
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      p <- plotGeneric(datfile.plot,
        i.range.y = NA,
        i.range.y.labels = NA,
        i.shapes = rep(21, NCOL(datfile.plot)),
        i.colors = colors.palette$colThresholds,
        i.fills = colors.palette$colThresholds,
        i.sizes = rep(3, NCOL(datfile.plot)),
        i.linetypes = rep("solid", NCOL(datfile.plot)),
        i.linesize = 1,
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0)
      )
      if (is.null(p)) {
        zfix <- NULL
      } else {
        z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
        zfix <- fixPlotly(
          z,
          names(datfile.plot),
          rep(TRUE, NCOL(datfile.plot)),
          rep(TRUE, NCOL(datfile.plot)),
          trloc("main.checkdescribe.seasons"),
          "value",
          rownames(datfile.plot)
        )
      }
    }
    zfix
  })

  output$tbdSscheme <- formattable::renderFormattable({
    datastability <- data_stability()
    if (is.null(datastability)) {
      datashow <- NULL
    } else {
      temp1 <- datastability$stability.seasons
      datashow <- formattable::formattable(temp1, apply(
        temp1, 2,
        function(noxneeded) {
          formattable::formatter("span",
            style = x ~ formattable::style(color = ifelse(x, "green", "red")),
            x ~ formattable::icontext(ifelse(x, "ok", "remove"), ifelse(x, trloc("graphs.yes"), trloc("graphs.no")))
          )
        }
      ))
    }
    datashow
  })

  output$tbdSdetailed <- DT::renderDataTable(
    {
      datastability <- data_stability()
      if (is.null(datastability)) {
        datashow <- NULL
      } else {
        datashow <- format(round(datastability$stability.data, 2), nsmall = 2)
        names(datashow) <- c(trloc("graphs.durationlower"), trloc("graphs.duration"), trloc("graphs.durationupper"), trloc("graphs.startlower"), trloc("graphs.start"), trloc("graphs.startupper"), trloc("graphs.epidemicpercentagelower"), trloc("graphs.epidemicpercentage"), trloc("graphs.epidemicpercentageupper"), trloc("graphs.prethreshold.short"), trloc("graphs.postthreshold"), trloc("graphs.mediumthreshold.short"), trloc("graphs.highthreshold.short"), trloc("graphs.veryhighthreshold.short"))
      }
      datashow
    },
    options = list(scrollX = TRUE, scrollY = "600px", paging = FALSE, dom = "Bfrtip", columnDefs = list(list(targets = "_all", class = "dt-right")))
  )

  output$tbdSdetailed_x <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      datastability <- data_stability()
      datashow <- datastability$stability.data
      names(datashow) <- c(trloc("graphs.durationlower"), trloc("graphs.duration"), trloc("graphs.durationupper"), trloc("graphs.startlower"), trloc("graphs.start"), trloc("graphs.startupper"), trloc("graphs.epidemicpercentagelower"), trloc("graphs.epidemicpercentage"), trloc("graphs.epidemicpercentageupper"), trloc("graphs.prethreshold.short"), trloc("graphs.postthreshold"), trloc("graphs.mediumthreshold.short"), trloc("graphs.highthreshold.short"), trloc("graphs.veryhighthreshold.short"))
      if (!is.null(datastability)) {
        exportData(
          i.data = datashow, i.file = file,
          i.sheet = substring(trloc("main.checkdescribe.stability"), 1, 32), i.rownames = trloc("main.checkdescribe.seasons"), i.format = "xlsx"
        )
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$tbdSdetailed_c <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      datastability <- data_stability()
      datashow <- datastability$stability.data
      names(datashow) <- c(trloc("graphs.durationlower"), trloc("graphs.duration"), trloc("graphs.durationupper"), trloc("graphs.startlower"), trloc("graphs.start"), trloc("graphs.startupper"), trloc("graphs.epidemicpercentagelower"), trloc("graphs.epidemicpercentage"), trloc("graphs.epidemicpercentageupper"), trloc("graphs.prethreshold.short"), trloc("graphs.postthreshold"), trloc("graphs.mediumthreshold.short"), trloc("graphs.highthreshold.short"), trloc("graphs.veryhighthreshold.short"))
      if (!is.null(datastability)) {
        exportData(
          i.data = datashow, i.file = file,
          i.sheet = substring(trloc("main.checkdescribe.stability"), 1, 32), i.rownames = trloc("main.checkdescribe.seasons"), i.format = "csv"
        )
      }
    },
    contentType = "text/csv"
  )

  output$tbdGoodness <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      return(NULL)
    } else {
      tabsetPanel(
        tabPanel(trloc("main.checkdescribe.goodness.indicators"), uiOutput("tbdGoodnessIndicators")),
        tabPanel(
          trloc("main.checkdescribe.goodness.summary"),
          formattable::formattableOutput("tbdGoodnessSummary"),
          fluidRow(
            column(8),
            column(
              2,
              if (zipPresent() & openxlsxPresent()) {
                downloadButton("tbdGoodnessSummary_x", "xlsx")
              } else if (!openxlsxPresent()) {
                shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
              } else if (.Platform$OS.type == "windows") {
                shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
              } else if (.Platform$OS.type == "unix") {
                shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
              }
            ),
            column(2, downloadButton("tbdGoodnessSummary_c", "csv"))
          )
        ),
        tabPanel(trloc("main.checkdescribe.goodness.graphs"), uiOutput("tbdGoodnessGraphs")),
        tabPanel(trloc("main.checkdescribe.goodness.intensity"), uiOutput("tbdGoodnessIntensity")),
        tabPanel(
          trloc("main.checkdescribe.goodness.detailed"),
          formattable::formattableOutput("tbdGoodnessDetailed"),
          fluidRow(
            column(8),
            column(
              2,
              if (zipPresent() & openxlsxPresent()) {
                downloadButton("tbdGoodnessDetailed_x", "xlsx")
              } else if (!openxlsxPresent()) {
                shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
              } else if (.Platform$OS.type == "windows") {
                shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
              } else if (.Platform$OS.type == "unix") {
                shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
              }
            ),
            column(2, downloadButton("tbdGoodnessDetailed_c", "csv"))
          )
        )
      )
    }
  })

  output$tbdGoodnessIndicators <- renderUI({
    good <- data_good_global()
    if (is.null(good)) {
      return(NULL)
    } else {
      fluidPage(
        fluidRow(
          column(width = 3, shinydashboard::valueBox(format(round(good$results["Sensitivity"], 2), nsmall = 2), trloc("graphs.sensitivity"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow")),
          column(width = 3, shinydashboard::valueBox(format(round(good$results["Specificity"], 2), nsmall = 2), trloc("graphs.specificity"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow")),
          column(width = 3, shinydashboard::valueBox(format(round(good$results["Positive predictive value"], 2), nsmall = 2), trloc("graphs.ppv"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow")),
          column(width = 3, shinydashboard::valueBox(format(round(good$results["Negative predictive value"], 2), nsmall = 2), trloc("graphs.npv"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow"))
        ),
        fluidRow(
          column(width = 4, shinydashboard::valueBox(format(round(good$results["Percent agreement"], 2), nsmall = 2), trloc("graphs.percent"), icon = icon("fas fa-heartbeat"), width = 12, color = "aqua")),
          column(width = 4, shinydashboard::valueBox(format(round(good$results["Matthews correlation coefficient"], 2), nsmall = 2), trloc("graphs.matthews"), icon = icon("fas fa-heartbeat"), width = 12, color = "aqua")),
          column(width = 4, shinydashboard::valueBox(format(round(good$results["Youdens Index"], 2), nsmall = 2), trloc("graphs.youden"), icon = icon("fas fa-heartbeat"), width = 12, color = "aqua"))
        )
      )
    }
  })

  output$tbdGoodnessSummary <- formattable::renderFormattable({
    good <- data_good_global()
    if (!is.null(good)) {
      temp1 <- as.data.frame(good$validity.data)
      temp1$Total <- good$results
      temp1 <- as.data.frame(t(temp1))[c("Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value", "Percent agreement", "Matthews correlation coefficient", "Youdens Index")]
      good.table <- formattable::formattable(temp1, list(
        "Sensitivity" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
        "Specificity" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
        "Positive predictive value" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
        "Negative predictive value" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
        "Percent agreement" = fixedColorBar(color = "#A5DBEB", fixedWidth = 100, alpha = 0.5),
        "Matthews correlation coefficient" = fixedColorBar(color = "#A5DBEB", fixedWidth = 100, alpha = 0.5),
        "Youdens Index" = fixedColorBar(color = "#A5DBEB", fixedWidth = 100, alpha = 0.5)
      ), digits = 2, format = "f")
      names(good.table) <- c(trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
      names(attr(good.table, "formattable")$format[[1]]) <- c(trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
    } else {
      temp1 <- data.frame(Error = trloc("ui.columnstwo"))
      good.table <- formattable::formattable(temp1)
    }
    good.table
  })

  output$tbdGoodnessSummary_x <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      good <- data_good_global()
      if (!is.null(good)) {
        temp1 <- as.data.frame(good$validity.data)
        temp1$Total <- good$results
        temp1 <- as.data.frame(t(temp1))[c("Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value", "Percent agreement", "Matthews correlation coefficient", "Youdens Index")]
        names(temp1) <- c(trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
        exportData(
          i.data = temp1, i.file = file,
          i.sheet = substring(trloc("app.globalgoodnesssummary"), 1, 32), i.rownames = trloc("selections.surveillance.season.label"), i.format = "xlsx"
        )
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$tbdGoodnessSummary_c <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      good <- data_good_global()
      if (!is.null(good)) {
        temp1 <- as.data.frame(good$validity.data)
        temp1$Total <- good$results
        temp1 <- as.data.frame(t(temp1))[c("Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value", "Percent agreement", "Matthews correlation coefficient", "Youdens Index")]
        names(temp1) <- c(trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
        exportData(
          i.data = temp1, i.file = file,
          i.sheet = substring(trloc("app.globalgoodnesssummary"), 1, 32), i.rownames = trloc("selections.surveillance.season.label"), i.format = "csv"
        )
      }
    },
    contentType = "text/csv"
  )

  output$tbdGoodnessIntensity <- renderUI({
    good <- data_good_global()
    peaks <- good$peaks
    if (is.null(good)) {
      return(NULL)
    } else {
      if (as.logical(input$advanced)) {
        fluidPage(
          fluidRow(
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 1] * 100, 2), nsmall = 1), "%"), trloc("graphs.baselinelevel"), icon = icon("fas fa-heartbeat"), width = 12, color = "lime")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 2] * 100, 2), nsmall = 1), "%"), trloc("graphs.lowlevel"), icon = icon("fas fa-thermometer-empty"), width = 12, color = "green")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 3] * 100, 2), nsmall = 1), "%"), trloc("graphs.mediumlevel"), icon = icon("fas fa-thermometer-quarter"), width = 12, color = "yellow")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 4] * 100, 2), nsmall = 1), "%"), trloc("graphs.highlevel"), icon = icon("fas fa-thermometer-half"), width = 12, color = "orange")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 5] * 100, 2), nsmall = 1), "%"), trloc("graphs.veryhighlevel"), icon = icon("fas fa-thermometer-three-quarters"), width = 12, color = "red"))
          ),
          fluidRow(
            column(width = 5, shinydashboard::valueBox(peaks$Count[peaks[, 1] == -1], trloc("graphs.totalseasons"), icon = icon("fas fa-heartbeat"), width = 12, color = "teal")),
            column(width = 5, shinydashboard::valueBox(peaks$Count[peaks[, 1] == 0], trloc("graphs.nodataseasons"), icon = icon("fas fa-heartbeat"), width = 12, color = "teal"))
          )
        )
      } else {
        fluidPage(
          fluidRow(
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 1] * 100, 2), nsmall = 1), "%"), trloc("graphs.baselinelevel"), icon = icon("fas fa-heartbeat"), width = 12, color = "lime")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 2] * 100, 2), nsmall = 1), "%"), trloc("graphs.lowlevel"), icon = icon("fas fa-thermometer-empty"), width = 12, color = "green")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 3] * 100, 2), nsmall = 1), "%"), trloc("graphs.mediumlevel"), icon = icon("fas fa-thermometer-quarter"), width = 12, color = "yellow")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 4] * 100, 2), nsmall = 1), "%"), trloc("graphs.highlevel"), icon = icon("fas fa-thermometer-half"), width = 12, color = "orange")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 5] * 100, 2), nsmall = 1), "%"), trloc("graphs.veryhighlevel"), icon = icon("fas fa-thermometer-three-quarters"), width = 12, color = "red"))
          ),
          fluidRow(
            column(width = 10, shinydashboard::valueBox(peaks$Count[peaks[, 1] == -1], trloc("graphs.totalseasons"), icon = icon("fas fa-heartbeat"), width = 12, color = "teal"))
          )
        )
      }
    }
  })

  output$tbdGoodnessDetailed <- formattable::renderFormattable({
    good <- data_good_global()
    if (!is.null(good)) {
      temp1 <- good$peaks.data
      temp1$Level <- as.character(temp1$Level)
      temp1$Description[temp1$Description == "Baseline"] <- trloc("graphs.baselinelevel")
      temp1$Description[temp1$Description == "Low"] <- trloc("graphs.lowlevel")
      temp1$Description[temp1$Description == "Medium"] <- trloc("graphs.mediumlevel")
      temp1$Description[temp1$Description == "High"] <- trloc("graphs.highlevel")
      temp1$Description[temp1$Description == "Very high"] <- trloc("graphs.veryhighlevel")
      currentpalette <- generatePalette(i.colThresholds = input$colThresholds, i.colLevels = input$colLevels)
      thr.c <- currentpalette$colThresholds
      lvl.n <- as.character(c(seq_len(5)))
      lvl.t <- c(trloc("graphs.baselinelevel"), trloc("graphs.lowlevel"), trloc("graphs.mediumlevel"), trloc("graphs.highlevel"), trloc("graphs.veryhighlevel"))
      lvl.c <- currentpalette$colLevels
      peaks.data <- formattable::formattable(temp1, list(
        "Epidemic threshold" = formattable::formatter("span", style = formattable::style(color = thr.c[1], font.weight = "bold")),
        "Medium threshold" = formattable::formatter("span", style = formattable::style(color = thr.c[2], font.weight = "bold")),
        "High threshold" = formattable::formatter("span", style = formattable::style(color = thr.c[3], font.weight = "bold")),
        "Very high threshold" = formattable::formatter("span", style = formattable::style(color = thr.c[4], font.weight = "bold")),
        "Level" = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(is.na(x), "grey", ifelse(x == lvl.n[1], lvl.c[1], ifelse(x == lvl.n[2], lvl.c[2], ifelse(x == lvl.n[3], lvl.c[3], ifelse(x == lvl.n[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold")),
        "Description" = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(is.na(x), "grey", ifelse(x == lvl.t[1], lvl.c[1], ifelse(x == lvl.t[2], lvl.c[2], ifelse(x == lvl.t[3], lvl.c[3], ifelse(x == lvl.t[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold"))
      ), digits = 2, format = "f")
      names(peaks.data) <- c(trloc("main.checkdescribe.goodness.detailed.peak"), trloc("main.checkdescribe.goodness.detailed.peakweek"), trloc("options.mem.thresholds.epidemic.label"), trloc("graphs.mediumthreshold"), trloc("graphs.highthreshold"), trloc("graphs.veryhighthreshold"), trloc("main.checkdescribe.goodness.level"), trloc("main.checkdescribe.goodness.description"))
      names(attr(peaks.data, "formattable")$format[[1]]) <- c(trloc("options.mem.thresholds.epidemic.label"), trloc("graphs.mediumthreshold"), trloc("graphs.highthreshold"), trloc("graphs.veryhighthreshold"), trloc("main.checkdescribe.goodness.level"), trloc("main.checkdescribe.goodness.description"))
    } else {
      temp1 <- data.frame(Error = trloc("ui.columnstwo"))
      peaks.data <- formattable::formattable(temp1)
    }
    peaks.data
  })

  output$tbdGoodnessDetailed_x <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      good <- data_good_global()
      if (!is.null(good)) {
        temp1 <- good$peaks.data
        temp1$Level <- as.character(temp1$Level)
        temp1$Description[temp1$Description == "Baseline"] <- trloc("graphs.baselinelevel")
        temp1$Description[temp1$Description == "Low"] <- trloc("graphs.lowlevel")
        temp1$Description[temp1$Description == "Medium"] <- trloc("graphs.mediumlevel")
        temp1$Description[temp1$Description == "High"] <- trloc("graphs.highlevel")
        temp1$Description[temp1$Description == "Very high"] <- trloc("graphs.veryhighlevel")
        names(temp1)[names(temp1) == "Peak"] <- trloc("main.checkdescribe.goodness.detailed.peak")
        names(temp1)[names(temp1) == "Peak week"] <- trloc("main.checkdescribe.goodness.detailed.peakweek")
        names(temp1)[names(temp1) == "Epidemic threshold"] <- trloc("options.mem.thresholds.epidemic.label")
        names(temp1)[names(temp1) == "Medium threshold"] <- trloc("graphs.mediumthreshold")
        names(temp1)[names(temp1) == "High threshold"] <- trloc("graphs.highthreshold")
        names(temp1)[names(temp1) == "Very high threshold"] <- trloc("graphs.veryhighthreshold")
        names(temp1)[names(temp1) == "Level"] <- trloc("graphs.level")
        names(temp1)[names(temp1) == "Description"] <- trloc("main.checkdescribe.goodness.description")
        exportData(
          i.data = temp1, i.file = file,
          i.sheet = substring(trloc("app.globalgoodnessintensity"), 1, 32), i.rownames = trloc("selections.surveillance.season.label"), i.format = "xlsx"
        )
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$tbdGoodnessDetailed_c <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      good <- data_good_global()
      if (!is.null(good)) {
        temp1 <- good$peaks.data
        temp1$Level <- as.character(temp1$Level)
        temp1$Description[temp1$Description == "Baseline"] <- trloc("graphs.baselinelevel")
        temp1$Description[temp1$Description == "Low"] <- trloc("graphs.lowlevel")
        temp1$Description[temp1$Description == "Medium"] <- trloc("graphs.mediumlevel")
        temp1$Description[temp1$Description == "High"] <- trloc("graphs.highlevel")
        temp1$Description[temp1$Description == "Very high"] <- trloc("graphs.veryhighlevel")
        names(temp1)[names(temp1) == "Peak"] <- trloc("main.checkdescribe.goodness.detailed.peak")
        names(temp1)[names(temp1) == "Peak week"] <- trloc("main.checkdescribe.goodness.detailed.peakweek")
        names(temp1)[names(temp1) == "Epidemic threshold"] <- trloc("options.mem.thresholds.epidemic.label")
        names(temp1)[names(temp1) == "Medium threshold"] <- trloc("graphs.mediumthreshold")
        names(temp1)[names(temp1) == "High threshold"] <- trloc("graphs.highthreshold")
        names(temp1)[names(temp1) == "Very high threshold"] <- trloc("graphs.veryhighthreshold")
        names(temp1)[names(temp1) == "Level"] <- trloc("graphs.level")
        names(temp1)[names(temp1) == "Description"] <- trloc("main.checkdescribe.goodness.description")
        exportData(
          i.data = temp1, i.file = file,
          i.sheet = substring(trloc("app.globalgoodnessintensity"), 1, 32), i.rownames = trloc("selections.surveillance.season.label"), i.format = "csv"
        )
      }
    },
    contentType = "text/csv"
  )

  output$tbdGoodnessGraphs <- renderUI({
    good <- data_good_global()
    if (is.null(good)) {
      return(NULL)
    } else {
      no.seasons <- NCOL(good$param.data)
      if (good$param.goodness.method == "sequential") se.seasons <- 3:no.seasons else se.seasons <- seq_len(no.seasons)
      nu.seasons <- (seq_len(no.seasons))[se.seasons]
      na.seasons <- (names(good$param.data))[se.seasons]
      do.call(
        tabsetPanel,
        lapply(na.seasons, function(s) {
          call("tabPanel", s, call("imageOutput", outputId = paste0("tbdGoodnessGraphs_", s), width = "auto", height = "auto"))
        })
      )
    }
  })

  #####################################
  ### MODEL TAB
  #####################################

  output$tbModel <- renderUI({
    datamodel <- data_model()
    moddata <- datamodel$param.data
    if (is.null(moddata)) {
      return(NULL)
    } else {
      if (as.logical(input$advanced)) {
        tabsetPanel(
          tabPanel(
            trloc("main.checkdescribe.data"),
            DT::dataTableOutput("tbmData"),
            fluidRow(
              column(8),
              column(
                2,
                if (zipPresent() & openxlsxPresent()) {
                  downloadButton("tbmData_x", "xlsx")
                } else if (!openxlsxPresent()) {
                  shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
                } else if (.Platform$OS.type == "windows") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                } else if (.Platform$OS.type == "unix") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
                }
              ),
              column(2, downloadButton("tbmData_c", "csv"))
            )
          ),
          tabPanel(trloc("main.checkdescribe.seasons"), plotlyOutput("tbmSeasons", width = "auto", height = "auto")),
          tabPanel(trloc("main.checkdescribe.series"), plotlyOutput("tbmSeries", width = "auto", height = "auto")),
          tabPanel(trloc("main.checkdescribe.timing"), uiOutput("tbmTiming")),
          tabPanel(trloc("main.model.mem"), uiOutput("tbmMem")),
          tabPanel(trloc("main.checkdescribe.goodness"), uiOutput("tbmGoodness")),
          tabPanel(trloc("main.model.optimize"), uiOutput("tbmOptimize"))
        )
      } else {
        tabsetPanel(
          tabPanel(
            trloc("main.checkdescribe.data"),
            DT::dataTableOutput("tbmData"),
            fluidRow(
              column(8),
              column(
                2,
                if (zipPresent() & openxlsxPresent()) {
                  downloadButton("tbmData_x", "xlsx")
                } else if (!openxlsxPresent()) {
                  shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
                } else if (.Platform$OS.type == "windows") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                } else if (.Platform$OS.type == "unix") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
                }
              ),
              column(2, downloadButton("tbmData_c", "csv"))
            )
          ),
          tabPanel(trloc("main.checkdescribe.seasons"), plotlyOutput("tbmSeasons", width = "auto", height = "auto")),
          tabPanel(trloc("main.checkdescribe.series"), plotlyOutput("tbmSeries", width = "auto", height = "auto")),
          tabPanel(trloc("main.checkdescribe.timing"), uiOutput("tbmTiming")),
          tabPanel(trloc("main.model.mem"), uiOutput("tbmMem")),
          tabPanel(trloc("main.checkdescribe.goodness"), uiOutput("tbmGoodness")),
          tabPanel(trloc("main.model.optimize"), uiOutput("tbmOptimizeA"))
        )
      }
    }
  })

  output$tbmData <- DT::renderDataTable(
    {
      datamodel <- data_model()
      if (is.null(datamodel)) {
        datatoshow <- data.frame(Message = "No data selected", row.names = NULL)
      } else {
        datatoshow <- format(round(datamodel$param.data, 2), nsmall = 2)
      }
      datatoshow
    },
    options = list(scrollX = TRUE, scrollY = "600px", paging = FALSE, dom = "Bfrtip", columnDefs = list(list(targets = "_all", class = "dt-right")))
  )

  output$tbmData_x <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      datamodel <- data_model()
      if (!is.null(datamodel)) {
        exportData(
          i.data = datamodel$param.data, i.file = file,
          i.sheet = substring(trloc("app.modeldata"), 1, 32), i.rownames = trloc("app.weekno"), i.format = "xlsx"
        )
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$tbmData_c <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      datamodel <- data_model()
      if (!is.null(datamodel)) {
        exportData(
          i.data = datamodel$param.data, i.file = file,
          i.sheet = substring(trloc("app.modeldata"), 1, 32), i.rownames = trloc("app.weekno"), i.format = "csv"
        )
      }
    },
    contentType = "text/csv"
  )

  output$tbmSeasons <- renderPlotly({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (is.null(datfile.plot)) {
      zfix <- NULL
    } else {
      e.thr <- datamodel$epidemic.thresholds
      i.thr <- datamodel$intensity.thresholds
      colors.palette <- generatePalette(
        i.number.series = NCOL(datfile.plot),
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      p <- plotSeasons(datfile.plot,
        i.epidemic.thr = e.thr,
        i.intensity.thr = i.thr,
        i.pre.epidemic = as.logical(input$preepidemicthr),
        i.post.epidemic = as.logical(input$postepidemicthr),
        i.intensity = as.logical(input$intensitythr),
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.type.threshold = as.numeric(input$typethreshold),
        i.tails.threshold = as.numeric(input$ntails),
        i.type.intensity = as.numeric(input$typeintensity),
        i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
        i.tails.intensity = as.numeric(input$ntails),
        i.type.curve = as.numeric(input$typeaveragecurve),
        i.level.curve = as.numeric(input$levelaveragecurve) / 100,
        i.type.other = as.numeric(input$typeother),
        i.level.other = as.numeric(input$levelother) / 100,
        i.method = as.numeric(input$method),
        i.param = as.numeric(input$param),
        i.centering = as.numeric(input$centering),
        i.n.max = as.numeric(input$nvalues),
        i.colObservedPoints = colors.palette$colObservedPoints,
        i.colSeasons = colors.palette$colSeasons,
        i.colThresholds = colors.palette$colThresholds,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0),
        i.use.t = as.logical(input$usetdistribution)
      )
      if (is.null(p)) {
        zfix <- NULL
      } else {
        z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
        zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
      }
    }
    zfix
  })

  output$tbmSeries <- renderPlotly({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (is.null(datfile.plot)) {
      zfix <- NULL
    } else {
      e.thr <- datamodel$epidemic.thresholds
      i.thr <- datamodel$intensity.thresholds
      colors.palette <- generatePalette(
        i.number.series = NA,
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      p <- plotSeries(
        i.data = datfile.plot,
        i.plot.timing = as.logical(input$plottiming),
        i.range.x = NA,
        i.pre.epidemic = as.logical(input$preepidemicthr),
        i.post.epidemic = as.logical(input$postepidemicthr),
        i.epidemic.thr = e.thr,
        i.intensity = as.logical(input$intensitythr),
        i.intensity.thr = i.thr,
        i.range.y = NA,
        i.replace.x.cr = TRUE,
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.type.threshold = as.numeric(input$typethreshold),
        i.tails.threshold = as.numeric(input$ntails),
        i.type.intensity = as.numeric(input$typeintensity),
        i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
        i.tails.intensity = as.numeric(input$ntails),
        i.type.curve = as.numeric(input$typeaveragecurve),
        i.level.curve = as.numeric(input$levelaveragecurve) / 100,
        i.type.other = as.numeric(input$typeother),
        i.level.other = as.numeric(input$levelother) / 100,
        i.method = as.numeric(input$method),
        i.param = as.numeric(input$param),
        i.centering = as.numeric(input$centering),
        i.n.max = as.numeric(input$nvalues),
        i.colObservedLines = colors.palette$colObservedLines,
        i.colThresholds = colors.palette$colThresholds,
        i.colObservedPoints = colors.palette$colObservedPoints,
        i.colEpidemic = colors.palette$colEpidemic,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0),
        i.use.t = as.logical(input$usetdistribution)
      )
      if (is.null(p)) {
        zfix <- NULL
      } else {
        z <- ggplotly(p$plot, width = as.numeric(values$gwidth2), height = as.numeric(values$gheight2))
        zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
      }
    }
    zfix
  })

  output$tbmTiming <- renderUI({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (is.null(datfile.plot)) {
      return(NULL)
    } else {
      tabnames <- names(datfile.plot)
      do.call(
        tabsetPanel,
        ## Create a set of tabPanel functions dependent on tabnames
        lapply(tabnames, function(s) {
          ## Populate the tabPanel with a dataTableOutput layout, with ID specific to the sample.
          ## Can also accommodate additional layout parts by adding additional call() to call("tabPanel")
          call("tabPanel", s, call("uiOutput", outputId = paste0("tbmTiming_", s), width = "auto", height = "auto"))
        })
      )
    }
  })

  output$tbmMem <- renderUI({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (is.null(datfile.plot)) {
      return(NULL)
    } else {
      tabsetPanel(
        tabPanel(trloc("main.model.mem.estimators"), uiOutput("tbmMemSummary")),
        tabPanel(trloc("main.model.mem.detailed"), verbatimTextOutput("tbmMemOutput")),
        tabPanel(trloc("main.model.mem.graphs"), uiOutput("tbmMemGraph"))
      )
    }
  })

  output$tbmMemSummary <- renderUI({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (is.null(datfile.plot)) {
      return(NULL)
    } else {
      fluidPage(
        fluidRow(
          column(width = 3, shinydashboard::valueBox(datamodel$n.seasons, trloc("graphs.modelseasons"), icon = icon("fas fa-heartbeat"), width = 12, color = "light-blue")),
          column(width = 3, shinydashboard::valueBox(datamodel$ci.start[2, 2], trloc("graphs.averagestart"), icon = icon("fas fa-heartbeat"), width = 12, color = "light-blue")),
          column(width = 3, shinydashboard::valueBox(format(round(datamodel$ci.length[1, 2], 2), nsmall = 1), trloc("graphs.averagelength"), icon = icon("fas fa-heartbeat"), width = 12, color = "light-blue")),
          column(width = 3, shinydashboard::valueBox(paste0(format(round(datamodel$ci.percent[2], 2), nsmall = 1), "%"), trloc("graphs.epidemicpercentage"), icon = icon("fas fa-heartbeat"), width = 12, color = "light-blue"))
        ),
        fluidRow(
          column(width = 3, shinydashboard::valueBox(format(round(datamodel$pre.post.intervals[1, 3], 2), nsmall = 1), trloc("options.mem.thresholds.epidemic.label"), icon = icon("fas fa-thermometer-empty"), width = 12, color = "green")),
          column(width = 3, shinydashboard::valueBox(format(round(datamodel$epi.intervals[1, 4], 2), nsmall = 1), trloc("graphs.mediumthreshold"), icon = icon("fas fa-thermometer-quarter"), width = 12, color = "yellow")),
          column(width = 3, shinydashboard::valueBox(format(round(datamodel$epi.intervals[2, 4], 2), nsmall = 1), trloc("graphs.highthreshold"), icon = icon("fas fa-thermometer-half"), width = 12, color = "orange")),
          column(width = 3, shinydashboard::valueBox(format(round(datamodel$epi.intervals[3, 4], 2), nsmall = 1), trloc("graphs.veryhighthreshold"), icon = icon("fas fa-thermometer-three-quarters"), width = 12, color = "red"))
        )
      )
    }
  })

  output$tbmMemOutput <- renderPrint({
    infile <- input$file
    indataset <- input$dataset
    readdata <- read_data()
    datfile <- readdata$datasetread
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (!is.null(datfile.plot)) {
      cat(trloc("main.checkdescribe.file"), ":\n\t", infile$name, "\n", sep = "")
      cat(trloc("selections.dataset"), ":\n\t", indataset, "\n", sep = "")
      cat(trloc("ui.log"), ":\n", sep = "")
      writeLines(paste("\t", capture.output(cat(readdata$datalog, sep = "")), sep = ""))
      cat(trloc("ui.memmodelsummary"), ":\n", sep = "")
      writeLines(paste("\t", capture.output(summary(datamodel)), sep = ""))
    } else {
      war.text <- as.data.frame(error = trloc("ui.memneedstwo"))
      names(war.text) <- NULL
      print(noquote(war.text), row.names = FALSE)
    }
  })

  output$tbmMemGraph <- renderUI({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (is.null(datfile.plot)) {
      return(NULL)
    } else {
      tabsetPanel(
        tabPanel(trloc("main.model.mem.graphs.moving"), plotlyOutput("tbmMemGraphMoving", width = "auto", height = "auto")),
        tabPanel(trloc("main.model.mem.graphs.average"), plotlyOutput("tbmMemGraphAverage", width = "auto", height = "auto"))
      )
    }
  })

  output$tbmMemGraphMoving <- renderPlotly({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (is.null(datfile.plot)) {
      zfix <- NULL
    } else {
      e.thr <- datamodel$epidemic.thresholds
      i.thr <- datamodel$intensity.thresholds
      datfile.plot <- data.frame(datamodel$moving.epidemics, row.names = rownames(datamodel$param.data))
      names(datfile.plot) <- names(datamodel$param.data)
      datfile.plot$dummy <- datamodel$typ.curve[, 2]
      names(datfile.plot)[names(datfile.plot) == "dummy"] <- trloc("graphs.averagecurve")
      colors.palette <- generatePalette(
        i.number.series = NCOL(datfile.plot),
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      p <- plotSeasons(datfile.plot,
        i.epidemic.thr = e.thr,
        i.intensity.thr = i.thr,
        i.pre.epidemic = as.logical(input$preepidemicthr),
        i.post.epidemic = as.logical(input$postepidemicthr),
        i.intensity = as.logical(input$intensitythr),
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.type.threshold = as.numeric(input$typethreshold),
        i.tails.threshold = as.numeric(input$ntails),
        i.type.intensity = as.numeric(input$typeintensity),
        i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
        i.tails.intensity = as.numeric(input$ntails),
        i.type.curve = as.numeric(input$typeaveragecurve),
        i.level.curve = as.numeric(input$levelaveragecurve) / 100,
        i.type.other = as.numeric(input$typeother),
        i.level.other = as.numeric(input$levelother) / 100,
        i.method = as.numeric(input$method),
        i.param = as.numeric(input$param),
        i.centering = as.numeric(input$centering),
        i.n.max = as.numeric(input$nvalues),
        i.colObservedPoints = colors.palette$colObservedPoints,
        i.colSeasons = colors.palette$colSeasons,
        i.colThresholds = colors.palette$colThresholds,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0),
        i.use.t = as.logical(input$usetdistribution)
      )
      if (is.null(p)) {
        zfix <- NULL
      } else {
        p0 <- p$plot +
          geom_point(
            x = datamodel$mean.start, y = datfile.plot[datamodel$mean.start, trloc("graphs.averagecurve")],
            color = colors.palette$colEpidemicStart, size = 2, fill = colors.palette$colEpidemicStart, shape = 21
          ) +
          geom_point(
            x = datamodel$mean.start + datamodel$mean.length - 1, y = datfile.plot[datamodel$mean.start + datamodel$mean.length - 1, trloc("graphs.averagecurve")],
            color = colors.palette$colEpidemicStop, size = 2, fill = colors.palette$colEpidemicStop, shape = 21
          ) +
          geom_vline(
            xintercept = datamodel$centered.start - 0.5,
            col = colors.palette$colEpidemicStart, linetype = "longdash", size = 0.5
          ) +
          geom_vline(
            xintercept = datamodel$centered.start + datamodel$centered.length - 1 + 0.5,
            col = colors.palette$colEpidemicStop, linetype = "longdash", size = 0.5
          )
        z <- ggplotly(p0, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
        # Change Average curve to: more width and dot stype
        z$x$data[[NCOL(datfile.plot)]]$line$width <- 2 * z$x$data[[NCOL(datfile.plot)]]$line$width
        z$x$data[[NCOL(datfile.plot)]]$line$dash <- "dot"
        # Rename name and text for vertical lines I've just added
        z$x$data[[2 * length(p$labels) + 1]]$name <- trloc("graphs.meanstart")
        z$x$data[[2 * length(p$labels) + 2]]$name <- trloc("graphs.meanend")
        z$x$data[[2 * length(p$labels) + 3]]$name <- trloc("graphs.startcentering")
        z$x$data[[2 * length(p$labels) + 4]]$name <- trloc("graphs.endcentering")
        z$x$data[[2 * length(p$labels) + 1]]$text <- paste(trloc("graphs.meanstart"), ": ", rownames(datfile.plot)[datamodel$mean.start], sep = "")
        z$x$data[[2 * length(p$labels) + 2]]$text <- paste(trloc("graphs.meanend"), ": ", rownames(datfile.plot)[datamodel$mean.start + datamodel$mean.length - 1], sep = "")
        z$x$data[[2 * length(p$labels) + 3]]$text <- paste(trloc("graphs.startcentering"), ": ", rownames(datfile.plot)[datamodel$centered.start], sep = "")
        z$x$data[[2 * length(p$labels) + 4]]$text <- paste(trloc("graphs.endcentering"), ": ", rownames(datfile.plot)[datamodel$centered.start + datamodel$centered.length - 1], sep = "")

        # And I need to rearrange the order of the z list for fixPlotly to work
        names(z$x$data) <- as.character(seq_len(2 * length(p$labels) + 4))
        z$x$data <- z$x$data[as.character(c(seq_len(length(p$labels)), 2 * length(p$labels) + seq_len(4), (length(p$labels) + 1):(2 * length(p$labels)), 2 * length(p$labels) + seq_len(4)))]
        names(z$x$data) <- NULL
        zfix <- fixPlotly(
          z,
          c(p$labels, c(trloc("graphs.meanstart"), trloc("graphs.meanend"), trloc("graphs.startcentering"), trloc("graphs.endcentering"))),
          c(p$haslines, FALSE, FALSE, TRUE, TRUE),
          c(p$haspoints, TRUE, TRUE, FALSE, FALSE),
          trloc("graphs.week"), "value", p$weeklabels
        )
      }
    }
    zfix
  })

  output$tbmMemGraphAverage <- renderPlotly({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (is.null(datfile.plot)) {
      zfix <- NULL
    } else {
      datfile.plot <- data.frame(Average = datamodel$typ.curve[, 2], row.names = rownames(datamodel$param.data))
      names(datfile.plot) <- trloc("graphs.averagecurve")
      e.thr <- datamodel$epidemic.thresholds
      i.thr <- datamodel$intensity.thresholds
      colors.palette <- generatePalette(
        i.number.series = NA,
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      p <- plotSurveillance(
        i.data = datfile.plot,
        i.week.report = rev(rownames(datfile.plot))[1],
        i.pre.epidemic = as.logical(input$preepidemicthr),
        i.post.epidemic = as.logical(input$postepidemicthr),
        i.start = as.logical(input$preepidemicthr),
        i.end = as.logical(input$postepidemicthr),
        i.epidemic.thr = e.thr,
        i.intensity = as.logical(input$intensitythr),
        i.intensity.thr = i.thr,
        i.mean.length = datamodel$mean.length,
        i.force.length = TRUE,
        i.force.equal = FALSE,
        i.force.start = datamodel$ci.start[2, 2],
        i.force.week.53 = FALSE,
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.colObservedLines = colors.palette$colObservedLines,
        i.colObservedPoints = colors.palette$colObservedPoints,
        i.colEpidemicStart = colors.palette$colEpidemicStart,
        i.colEpidemicStop = colors.palette$colEpidemicStop,
        i.colThresholds = colors.palette$colThresholds,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0)
      )
      if (is.null(p)) {
        zfix <- NULL
      } else {
        z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
        zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
      }
    }
    zfix
  })

  output$tbmGoodness <- renderUI({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (is.null(datfile.plot)) {
      return(NULL)
    } else {
      if (as.logical(input$advanced)) {
        tabsetPanel(
          tabPanel(trloc("main.checkdescribe.goodness.indicators"), uiOutput("tbmGoodnessIndicators")),
          tabPanel(
            trloc("main.checkdescribe.goodness.summary"),
            formattable::formattableOutput("tbmGoodnessSummary"),
            fluidRow(
              column(8),
              column(
                2,
                if (zipPresent() & openxlsxPresent()) {
                  downloadButton("tbmGoodnessSummary_x", "xlsx")
                } else if (!openxlsxPresent()) {
                  shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
                } else if (.Platform$OS.type == "windows") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                } else if (.Platform$OS.type == "unix") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
                }
              ),
              column(2, downloadButton("tbmGoodnessSummary_c", "csv"))
            )
          ),
          tabPanel(trloc("main.model.goodness.graphs"), uiOutput("tbmGoodnessGraphs")),
          tabPanel(trloc("main.checkdescribe.goodness.intensity"), uiOutput("tbmGoodnessIntensity")),
          tabPanel(
            trloc("main.checkdescribe.goodness.detailed"),
            formattable::formattableOutput("tbmGoodnessDetailed"),
            fluidRow(
              column(8),
              column(
                2,
                if (zipPresent() & openxlsxPresent()) {
                  downloadButton("tbmGoodnessDetailed_x", "xlsx")
                } else if (!openxlsxPresent()) {
                  shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
                } else if (.Platform$OS.type == "windows") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                } else if (.Platform$OS.type == "unix") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
                }
              ),
              column(2, downloadButton("tbmGoodnessDetailed_c", "csv"))
            )
          )
        )
      } else {
        tabsetPanel(
          tabPanel(trloc("main.checkdescribe.goodness.indicators"), uiOutput("tbmGoodnessIndicators")),
          tabPanel(
            trloc("main.checkdescribe.goodness.summary"),
            formattable::formattableOutput("tbmGoodnessSummary"),
            fluidRow(
              column(8),
              column(
                2,
                if (zipPresent() & openxlsxPresent()) {
                  downloadButton("tbmGoodnessSummary_x", "xlsx")
                } else if (!openxlsxPresent()) {
                  shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
                } else if (.Platform$OS.type == "windows") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                } else if (.Platform$OS.type == "unix") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
                }
              ),
              column(2, downloadButton("tbmGoodnessSummary_c", "csv"))
            )
          ),
          tabPanel(trloc("main.checkdescribe.goodness.intensity"), uiOutput("tbmGoodnessIntensity")),
          tabPanel(
            trloc("main.checkdescribe.goodness.detailed"),
            formattable::formattableOutput("tbmGoodnessDetailed"),
            fluidRow(
              column(8),
              column(
                2,
                if (zipPresent() & openxlsxPresent()) {
                  downloadButton("tbmGoodnessDetailed_x", "xlsx")
                } else if (!openxlsxPresent()) {
                  shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
                } else if (.Platform$OS.type == "windows") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
                } else if (.Platform$OS.type == "unix") {
                  shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
                }
              ),
              column(2, downloadButton("tbmGoodnessDetailed_c", "csv"))
            )
          )
        )
      }
    }
  })

  output$tbmGoodnessIndicators <- renderUI({
    good <- data_good_model()
    if (is.null(good)) {
      return(NULL)
    } else {
      fluidPage(
        fluidRow(
          column(width = 3, shinydashboard::valueBox(format(round(good$results["Sensitivity"], 2), nsmall = 2), trloc("graphs.sensitivity"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow")),
          column(width = 3, shinydashboard::valueBox(format(round(good$results["Specificity"], 2), nsmall = 2), trloc("graphs.specificity"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow")),
          column(width = 3, shinydashboard::valueBox(format(round(good$results["Positive predictive value"], 2), nsmall = 2), trloc("graphs.ppv"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow")),
          column(width = 3, shinydashboard::valueBox(format(round(good$results["Negative predictive value"], 2), nsmall = 2), trloc("graphs.npv"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow"))
        ),
        fluidRow(
          column(width = 4, shinydashboard::valueBox(format(round(good$results["Percent agreement"], 2), nsmall = 2), trloc("graphs.percent"), icon = icon("fas fa-heartbeat"), width = 12, color = "aqua")),
          column(width = 4, shinydashboard::valueBox(format(round(good$results["Matthews correlation coefficient"], 2), nsmall = 2), trloc("graphs.matthews"), icon = icon("fas fa-heartbeat"), width = 12, color = "aqua")),
          column(width = 4, shinydashboard::valueBox(format(round(good$results["Youdens Index"], 2), nsmall = 2), trloc("graphs.youden"), icon = icon("fas fa-heartbeat"), width = 12, color = "aqua"))
        )
      )
    }
  })

  output$tbmGoodnessSummary <- formattable::renderFormattable({
    good <- data_good_model()
    if (!is.null(good)) {
      temp1 <- as.data.frame(good$validity.data)
      temp1$Total <- good$results
      temp1 <- as.data.frame(t(temp1))[c("Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value", "Percent agreement", "Matthews correlation coefficient", "Youdens Index")]
      good.table <- formattable::formattable(temp1, list(
        "Sensitivity" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
        "Specificity" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
        "Positive predictive value" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
        "Negative predictive value" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
        "Percent agreement" = fixedColorBar(color = "#A5DBEB", fixedWidth = 100, alpha = 0.5),
        "Matthews correlation coefficient" = fixedColorBar(color = "#A5DBEB", fixedWidth = 100, alpha = 0.5),
        "Youdens Index" = fixedColorBar(color = "#A5DBEB", fixedWidth = 100, alpha = 0.5)
      ), digits = 2, format = "f")
      names(good.table) <- c(trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
      names(attr(good.table, "formattable")$format[[1]]) <- c(trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
    } else {
      temp1 <- data.frame(Error = trloc("ui.columnstwo"))
      good.table <- formattable::formattable(temp1)
    }
    good.table
  })

  output$tbmGoodnessSummary_x <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      good <- data_good_model()
      if (!is.null(good)) {
        temp1 <- as.data.frame(good$validity.data)
        temp1$Total <- good$results
        temp1 <- as.data.frame(t(temp1))[c("Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value", "Percent agreement", "Matthews correlation coefficient", "Youdens Index")]
        names(temp1) <- c(trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
        exportData(
          i.data = temp1, i.file = file,
          i.sheet = substring(trloc("app.modelgoodnesssummary"), 1, 32), i.rownames = trloc("selections.surveillance.season.label"), i.format = "xlsx"
        )
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$tbmGoodnessSummary_c <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      good <- data_good_model()
      if (!is.null(good)) {
        temp1 <- as.data.frame(good$validity.data)
        temp1$Total <- good$results
        temp1 <- as.data.frame(t(temp1))[c("Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value", "Percent agreement", "Matthews correlation coefficient", "Youdens Index")]
        names(temp1) <- c(trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
        exportData(
          i.data = temp1, i.file = file,
          i.sheet = substring(trloc("app.modelgoodnesssummary"), 1, 32), i.rownames = trloc("selections.surveillance.season.label"), i.format = "csv"
        )
      }
    },
    contentType = "text/csv"
  )

  output$tbmGoodnessGraphs <- renderUI({
    good <- data_good_model()
    if (is.null(good)) {
      return(NULL)
    } else {
      no.seasons <- NCOL(good$param.data)
      if (good$param.goodness.method == "sequential") se.seasons <- 3:no.seasons else se.seasons <- seq_len(no.seasons)
      nu.seasons <- (seq_len(no.seasons))[se.seasons]
      na.seasons <- (names(good$param.data))[se.seasons]
      do.call(
        tabsetPanel,
        lapply(na.seasons, function(s) {
          cat("reactive/data_good_model> generating ", paste0("tbmGoodnessGraphs_", s), "\n")
          call("tabPanel", s, call("imageOutput", outputId = paste0("tbmGoodnessGraphs_", s), width = "auto", height = "auto"))
        })
      )
    }
  })

  output$tbmGoodnessIntensity <- renderUI({
    good <- data_good_model()
    peaks <- good$peaks
    if (is.null(good)) {
      return(NULL)
    } else {
      if (as.logical(input$advanced)) {
        fluidPage(
          fluidRow(
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 1] * 100, 2), nsmall = 1), "%"), trloc("graphs.baselinelevel"), icon = icon("fas fa-heartbeat"), width = 12, color = "lime")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 2] * 100, 2), nsmall = 1), "%"), trloc("graphs.lowlevel"), icon = icon("fas fa-thermometer-empty"), width = 12, color = "green")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 3] * 100, 2), nsmall = 1), "%"), trloc("graphs.mediumlevel"), icon = icon("fas fa-thermometer-quarter"), width = 12, color = "yellow")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 4] * 100, 2), nsmall = 1), "%"), trloc("graphs.highlevel"), icon = icon("fas fa-thermometer-half"), width = 12, color = "orange")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 5] * 100, 2), nsmall = 1), "%"), trloc("graphs.veryhighlevel"), icon = icon("fas fa-thermometer-three-quarters"), width = 12, color = "red"))
          ),
          fluidRow(
            column(width = 5, shinydashboard::valueBox(peaks$Count[peaks[, 1] == -1], trloc("graphs.totalseasons"), icon = icon("fas fa-heartbeat"), width = 12, color = "teal")),
            column(width = 5, shinydashboard::valueBox(peaks$Count[peaks[, 1] == 0], trloc("graphs.nodataseasons"), icon = icon("fas fa-heartbeat"), width = 12, color = "teal"))
          )
        )
      } else {
        fluidPage(
          fluidRow(
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 1] * 100, 2), nsmall = 1), "%"), trloc("graphs.baselinelevel"), icon = icon("fas fa-heartbeat"), width = 12, color = "lime")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 2] * 100, 2), nsmall = 1), "%"), trloc("graphs.lowlevel"), icon = icon("fas fa-thermometer-empty"), width = 12, color = "green")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 3] * 100, 2), nsmall = 1), "%"), trloc("graphs.mediumlevel"), icon = icon("fas fa-thermometer-quarter"), width = 12, color = "yellow")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 4] * 100, 2), nsmall = 1), "%"), trloc("graphs.highlevel"), icon = icon("fas fa-thermometer-half"), width = 12, color = "orange")),
            column(width = 2, shinydashboard::valueBox(paste0(format(round(peaks$Percentage[peaks[, 1] == 5] * 100, 2), nsmall = 1), "%"), trloc("graphs.veryhighlevel"), icon = icon("fas fa-thermometer-three-quarters"), width = 12, color = "red"))
          ),
          fluidRow(
            column(width = 10, shinydashboard::valueBox(peaks$Count[peaks[, 1] == -1], trloc("graphs.totalseasons"), icon = icon("fas fa-heartbeat"), width = 12, color = "teal"))
          )
        )
      }
    }
  })

  output$tbmGoodnessDetailed <- formattable::renderFormattable({
    good <- data_good_model()
    if (!is.null(good)) {
      temp1 <- good$peaks.data
      temp1$Level <- as.character(temp1$Level)
      temp1$Description[temp1$Description == "Baseline"] <- trloc("graphs.baselinelevel")
      temp1$Description[temp1$Description == "Low"] <- trloc("graphs.lowlevel")
      temp1$Description[temp1$Description == "Medium"] <- trloc("graphs.mediumlevel")
      temp1$Description[temp1$Description == "High"] <- trloc("graphs.highlevel")
      temp1$Description[temp1$Description == "Very high"] <- trloc("graphs.veryhighlevel")
      currentpalette <- generatePalette(i.colThresholds = input$colThresholds, i.colLevels = input$colLevels)
      thr.c <- currentpalette$colThresholds
      lvl.n <- as.character(c(seq_len(5)))
      lvl.t <- c(trloc("graphs.baselinelevel"), trloc("graphs.lowlevel"), trloc("graphs.mediumlevel"), trloc("graphs.highlevel"), trloc("graphs.veryhighlevel"))
      lvl.c <- currentpalette$colLevels
      peaks.data <- formattable::formattable(temp1, list(
        "Epidemic threshold" = formattable::formatter("span", style = formattable::style(color = thr.c[1], font.weight = "bold")),
        "Medium threshold" = formattable::formatter("span", style = formattable::style(color = thr.c[2], font.weight = "bold")),
        "High threshold" = formattable::formatter("span", style = formattable::style(color = thr.c[3], font.weight = "bold")),
        "Very high threshold" = formattable::formatter("span", style = formattable::style(color = thr.c[4], font.weight = "bold")),
        "Level" = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(is.na(x), "grey", ifelse(x == lvl.n[1], lvl.c[1], ifelse(x == lvl.n[2], lvl.c[2], ifelse(x == lvl.n[3], lvl.c[3], ifelse(x == lvl.n[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold")),
        "Description" = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(is.na(x), "grey", ifelse(x == lvl.t[1], lvl.c[1], ifelse(x == lvl.t[2], lvl.c[2], ifelse(x == lvl.t[3], lvl.c[3], ifelse(x == lvl.t[4], lvl.c[4], lvl.c[5]))))), font.weight = "bold"))
      ), digits = 2, format = "f")
      names(peaks.data) <- c(trloc("main.checkdescribe.goodness.detailed.peak"), trloc("main.checkdescribe.goodness.detailed.peakweek"), trloc("options.mem.thresholds.epidemic.label"), trloc("graphs.mediumthreshold"), trloc("graphs.highthreshold"), trloc("graphs.veryhighthreshold"), trloc("main.checkdescribe.goodness.level"), trloc("main.checkdescribe.goodness.description"))
      names(attr(peaks.data, "formattable")$format[[1]]) <- c(trloc("options.mem.thresholds.epidemic.label"), trloc("graphs.mediumthreshold"), trloc("graphs.highthreshold"), trloc("graphs.veryhighthreshold"), trloc("main.checkdescribe.goodness.level"), trloc("main.checkdescribe.goodness.description"))
    } else {
      temp1 <- data.frame(Error = trloc("ui.columnstwo"))
      peaks.data <- formattable::formattable(temp1)
    }
    peaks.data
  })

  output$tbmGoodnessDetailed_x <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      good <- data_good_model()
      if (!is.null(good)) {
        temp1 <- good$peaks.data
        temp1$Level <- as.character(temp1$Level)
        temp1$Description[temp1$Description == "Baseline"] <- trloc("graphs.baselinelevel")
        temp1$Description[temp1$Description == "Low"] <- trloc("graphs.lowlevel")
        temp1$Description[temp1$Description == "Medium"] <- trloc("graphs.mediumlevel")
        temp1$Description[temp1$Description == "High"] <- trloc("graphs.highlevel")
        temp1$Description[temp1$Description == "Very high"] <- trloc("graphs.veryhighlevel")
        names(temp1)[names(temp1) == "Peak"] <- trloc("main.checkdescribe.goodness.detailed.peak")
        names(temp1)[names(temp1) == "Peak week"] <- trloc("main.checkdescribe.goodness.detailed.peakweek")
        names(temp1)[names(temp1) == "Epidemic threshold"] <- trloc("options.mem.thresholds.epidemic.label")
        names(temp1)[names(temp1) == "Medium threshold"] <- trloc("graphs.mediumthreshold")
        names(temp1)[names(temp1) == "High threshold"] <- trloc("graphs.highthreshold")
        names(temp1)[names(temp1) == "Very high threshold"] <- trloc("graphs.veryhighthreshold")
        names(temp1)[names(temp1) == "Level"] <- trloc("graphs.level")
        names(temp1)[names(temp1) == "Description"] <- trloc("main.checkdescribe.goodness.description")
        exportData(
          i.data = temp1, i.file = file,
          i.sheet = substring(trloc("app.modelgoodnessintensity"), 1, 32), i.rownames = trloc("selections.surveillance.season.label"), i.format = "xlsx"
        )
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  output$tbmGoodnessDetailed_c <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      good <- data_good_model()
      if (!is.null(good)) {
        temp1 <- good$peaks.data
        temp1$Level <- as.character(temp1$Level)
        temp1$Description[temp1$Description == "Baseline"] <- trloc("graphs.baselinelevel")
        temp1$Description[temp1$Description == "Low"] <- trloc("graphs.lowlevel")
        temp1$Description[temp1$Description == "Medium"] <- trloc("graphs.mediumlevel")
        temp1$Description[temp1$Description == "High"] <- trloc("graphs.highlevel")
        temp1$Description[temp1$Description == "Very high"] <- trloc("graphs.veryhighlevel")
        names(temp1)[names(temp1) == "Peak"] <- trloc("main.checkdescribe.goodness.detailed.peak")
        names(temp1)[names(temp1) == "Peak week"] <- trloc("main.checkdescribe.goodness.detailed.peakweek")
        names(temp1)[names(temp1) == "Epidemic threshold"] <- trloc("options.mem.thresholds.epidemic.label")
        names(temp1)[names(temp1) == "Medium threshold"] <- trloc("graphs.mediumthreshold")
        names(temp1)[names(temp1) == "High threshold"] <- trloc("graphs.highthreshold")
        names(temp1)[names(temp1) == "Very high threshold"] <- trloc("graphs.veryhighthreshold")
        names(temp1)[names(temp1) == "Level"] <- trloc("graphs.level")
        names(temp1)[names(temp1) == "Description"] <- trloc("main.checkdescribe.goodness.description")
        exportData(
          i.data = temp1, i.file = file,
          i.sheet = substring(trloc("app.modelgoodnessintensity"), 1, 32), i.rownames = trloc("selections.surveillance.season.label"), i.format = "csv"
        )
      }
    },
    contentType = "text/csv"
  )

  output$tbmOptimize <- renderUI({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (is.null(datfile.plot)) {
      return(NULL)
    } else {
      tabsetPanel(
        tabPanel(trloc("main.model.optimize.manual"), uiOutput("tbmOptimizeM")),
        tabPanel(trloc("main.model.optimize.automatic"), uiOutput("tbmOptimizeA"))
      )
    }
  })

  output$tbmOptimizeM <- renderUI({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (is.null(datfile.plot)) {
      return(NULL)
    } else {
      tabnames <- names(datfile.plot)
      if (length(tabnames) < 2) {
        return(NULL)
      } else {
        do.call(
          tabsetPanel,
          c(
            lapply(tabnames, function(s) {
              call("tabPanel", s, call("uiOutput", outputId = paste0("tbmOptimizeM_", as.character(s))))
            }),
            list(
              tabPanel(trloc("main.model.optimize.manual.startend"), tableOutput("tbmOptimizeMstartend")),
              tabPanel(trloc("main.model.optimize.manual.clicks"), tableOutput("tbmOptimizeMclicks")),
              tabPanel(trloc("main.model.optimize.manual.results"), uiOutput("tbmOptimizeMresults"))
            )
          )
        )
      }
    }
  })

  output$tbmOptimizeMstartend <- renderTable({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (NROW(values$clickdata) > 0) {
      etwo <- extractTwo(values$clickdata, "weekno", "season")
      etwo <- merge(etwo, data.frame(id.tail = c(1, 2), point = c(trloc("graphs.start"), trloc("graphs.end")), stringsAsFactors = FALSE), by = "id.tail")
      optr <- subset(etwo, etwo$season %in% names(datfile.plot))[c("season", "weekna", "point", paste0(names(datfile.plot), "_fixed"))]
      optr <- optr %>%
        dplyr::arrange(season, desc(point)) %>%
        as.data.frame()
      names(optr)[seq_len(3)] <- c(trloc("selections.surveillance.season.label"), trloc("graphs.week"), trloc("graphs.point"))
      names(optr)[4:(NCOL(datfile.plot) + 3)] <- names(datfile.plot)
    } else {
      optr <- NULL
    }
    optr
  })

  output$tbmOptimizeMclicks <- renderTable({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (NROW(values$clickdata) > 0) {
      clickd <- values$clickdata
      optr <- subset(clickd, clickd$season %in% names(datfile.plot))[c("season", "weekna", "weekno", paste0(names(datfile.plot), "_fixed"))]
      optr <- optr %>%
        dplyr::arrange(season, weekno) %>%
        select(-weekno)
      as.data.frame
      names(optr)[seq_len(2)] <- c(trloc("selections.surveillance.season.label"), trloc("graphs.week"))
      names(optr)[3:(NCOL(datfile.plot) + 2)] <- names(datfile.plot)
    } else {
      optr <- NULL
    }
    optr
  })

  output$tbmOptimizeMresults <- renderUI({
    datamodel <- data_model()
    datfile.plot <- datamodel$param.data
    if (NROW(values$clickdata) > 0) {
      etwo <- extractTwo(values$clickdata, "weekno", "season")
      etwot <- etwo %>%
        select(id.tail, season, weekno) %>%
        tidyr::spread(id.tail, weekno, drop = FALSE, fill = NA)
      if (NCOL(datfile.plot) > 2) {
        if (all(names(datfile.plot) %in% etwo$season) & NCOL(etwot) == 3 & sum(is.na(etwot)) == 0) {
          i.data <- values$plotdata[grepl("^.*_fixed$", names(values$plotdata))]
          names(i.data) <- sub("_fixed", "", names(i.data), fixed = TRUE)
          i.data <- i.data[names(i.data) %in% names(datfile.plot)]
          row.names(i.data) <- values$plotdata$weekna
          tfile <- tempfile()
          tfile.div <- extractPfe(tfile)

          i.param.values <- seq(input$paramrange[1], input$paramrange[2], by = 0.1)
          i.graph <- TRUE
          i.graph.file <- TRUE
          i.graph.file.name <- tfile.div$name
          i.graph.title <- ""
          i.graph.subtitle <- ""
          i.output <- tfile.div$path

          semanas <- dim(i.data)[1]
          anios <- dim(i.data)[2]
          nombre.semana <- rownames(i.data)
          nombre.anios <- colnames(i.data)
          numero.semana <- seq_len(semanas)
          n.values <- length(i.param.values)

          i.timing.1 <- array(dim = c(anios, 2))
          resultados.i <- array(
            dim = c(anios, 15, n.values),
            dimnames = list(year = nombre.anios, indicator = LETTERS[seq_len(15)], parameter = i.param.values)
          )

          for (i in seq_len(anios)) {
            cur <- i.data[i]
            itsnotok <- TRUE
            i.timing.1.1 <- etwo$weekno[etwo$season == nombre.anios[i] & etwo$id.tail == 1]
            i.timing.1.2 <- etwo$weekno[etwo$season == nombre.anios[i] & etwo$id.tail == 2]
            i.timing.1.i <- c(i.timing.1.1, i.timing.1.2)
            i.timing.1[i, ] <- i.timing.1.i
            curva.map <- mem:::calcular.map(as.vector(as.matrix(cur)))
            for (j in seq_len(n.values)) {
              i.param.deteccion <- i.param.values[j]
              i.param.deteccion.label <- format(round(i.param.deteccion, 1), digits = 3, nsmall = 1)
              i.timing.2 <- mem:::calcular.optimo(curva.map, 2, i.param.deteccion)$resultados[4:5]
              resultado.j <- mem:::calcular.indicadores.2.timings(cur, i.timing.1.i,
                i.timing.2,
                i.timing.labels = c("inspection", i.param.deteccion.label),
                i.graph.title = "Comparing",
                i.graph.file = FALSE
              )$indicadores
              resultados.i[i, , j] <- as.numeric(resultado.j)
            }
          }
          resultado <- data.frame(apply(resultados.i, c(3, 2), sum, na.rm = TRUE))
          # sensibilidad
          resultado[7] <- resultado[3] / (resultado[3] + resultado[6])
          # especificidad
          resultado[8] <- resultado[5] / (resultado[5] + resultado[4])
          # vpp
          resultado[9] <- resultado[3] / (resultado[3] + resultado[4])
          # vpn
          resultado[10] <- resultado[5] / (resultado[5] + resultado[6])
          # positive likehood ratio
          resultado[11] <- resultado[7] / (1 - resultado[8])
          # negative likehood ratio
          resultado[12] <- (1 - resultado[7]) / resultado[8]
          # percentage agreement/accuracy
          resultado[13] <- (resultado[3] + resultado[5]) / (resultado[3] + resultado[4] + resultado[5] + resultado[6])
          # Matthews correlation coefficient
          resultado[14] <- (resultado[3] * resultado[5] - resultado[4] * resultado[6]) / sqrt((resultado[3] + resultado[4]) * (resultado[3] + resultado[6]) * (resultado[5] + resultado[4]) * (resultado[5] + resultado[6]))
          # Youdens Index
          resultado[15] <- resultado[7] + resultado[8] - 1

          resultado[resultado == "NaN"] <- NA

          resultados <- data.frame(value = i.param.values, resultado)
          names(resultados) <- c("value", tolower(colnames(resultado.j)))

          if (!any(!is.na(resultados$sensitivity)) | !any(!is.na(resultados$specificity))) {
            rankings.1 <- NA
            optimo.1 <- NA
          } else {
            rankings.1 <- rank(-resultados$sensitivity, na.last = TRUE) + rank(-resultados$specificity, na.last = TRUE)
            optimo.1 <- i.param.values[which.min(rankings.1)]
          }
          if (!any(!is.na(resultados$sensitivity)) | !any(!is.na(resultados$specificity))) {
            rankings.2 <- NA
            optimo.2 <- NA
          } else {
            rankings.2 <- rank(-resultados$sensitivity * resultados$specificity, na.last = TRUE)
            optimo.2 <- i.param.values[which.min(rankings.2)]
          }
          if (!any(!is.na(resultados$positive.likehood.ratio))) {
            rankings.3 <- NA
            optimo.3 <- NA
          } else {
            rankings.3 <- rank(-resultados$positive.likehood.ratio, na.last = TRUE)
            optimo.3 <- i.param.values[which.min(rankings.3)]
          }
          if (!any(!is.na(resultados$negative.likehood.ratio))) {
            rankings.4 <- NA
            optimo.4 <- NA
          } else {
            rankings.4 <- rank(-resultados$negative.likehood.ratio, na.last = TRUE)
            optimo.4 <- i.param.values[which.min(rankings.4)]
          }
          if (!any(!is.na(resultados$sensitivity)) | !any(!is.na(resultados$specificity))) {
            rankings.5 <- NA
            optimo.5 <- NA
          } else {
            qf <- abs(resultados$sensitivity - resultados$specificity)
            qe <- 2 - resultados$sensitivity - resultados$specificity
            qs <- (1 - resultados$sensitivity)^2 + (1 - resultados$specificity)^2
            rankings.5 <- rank(qf) + rank(qe) + rank(qs)
            optimo.5 <- i.param.values[which.min(rankings.5)]
          }
          if (!any(!is.na(resultados$percent.agreement))) {
            rankings.6 <- NA
            optimo.6 <- NA
          } else {
            rankings.6 <- rank(-resultados$percent.agreement, na.last = TRUE)
            optimo.6 <- i.param.values[which.min(rankings.6)]
          }
          if (!any(!is.na(resultados$matthews.correlation.coefficient))) {
            rankings.7 <- NA
            optimo.7 <- NA
          } else {
            rankings.7 <- rank(-resultados$matthews.correlation.coefficient, na.last = TRUE)
            optimo.7 <- i.param.values[which.min(rankings.7)]
          }
          if (!any(!is.na(resultados$youdens.index))) {
            rankings.8 <- NA
            optimo.8 <- NA
          } else {
            rankings.8 <- rank(-resultados$youdens.index, na.last = TRUE)
            optimo.8 <- i.param.values[which.min(rankings.8)]
          }


          optimum <- data.frame(
            pos.likehood = optimo.3, neg.likehood = optimo.4, aditive = optimo.1, multiplicative = optimo.2,
            mixed = optimo.5, percent = optimo.6, matthews = optimo.7, youden = optimo.8
          )

          rankings <- data.frame(
            pos.likehood = rankings.3, neg.likehood = rankings.4, aditive = rankings.1, multiplicative = rankings.2,
            mixed = rankings.5, percent = rankings.6, matthews = rankings.7, youden = rankings.8
          )


          optimum.by.inspection.output <- list(
            optimum = optimum,
            rankings = rankings,
            insp.data = resultados,
            param.data = i.data,
            param.param.values = i.param.values,
            param.graph = i.graph,
            param.graph.file = i.graph.file,
            param.graph.file.name = i.graph.file.name,
            param.graph.title = i.graph.title,
            param.graph.subtitle = i.graph.subtitle,
            param.output = i.output
          )

          # Graph all data
          if (i.graph) {
            if (i.graph.file.name == "") graph.name <- "inspection analysis" else graph.name <- i.graph.file.name


            if (i.graph.subtitle != "") graph.title <- paste(i.graph.subtitle, " - ", graph.title, sep = "")
            if (i.graph.title != "") graph.title <- paste(i.graph.title, "\n", graph.title, sep = "")

            all.graph.names <- data.frame()

            for (i in seq_len(anios)) {
              graph.title <- nombre.anios[i]

              all.graph.names <- rbind(all.graph.names, data.frame(season = graph.title, file = paste0(i.output, "/", graph.name, " - ", i, ".png"), stringsAsFactors = FALSE))

              cur <- i.data[i]
              i.timing.1.i <- i.timing.1[i, ]
              curva.map <- mem:::calcular.map(as.vector(as.matrix(cur)))
              i.param.deteccion <- optimum$matthews
              i.param.deteccion.label <- format(round(i.param.deteccion, 1), digits = 3, nsmall = 1)
              i.timing.2 <- mem:::calcular.optimo(curva.map, 2, i.param.deteccion)$resultados[4:5]
              dummmmyyyy <- mem:::calcular.indicadores.2.timings(cur, i.timing.1.i, i.timing.2,
                i.timing.labels = c("inspection", i.param.deteccion.label),
                i.output = i.output,
                i.graph.title = graph.title,
                i.graph.file = i.graph.file,
                i.graph.file.name = paste(graph.name, " - ", i, sep = "")
              )
            }

            values$optimizegraphs <- all.graph.names
          }

          lapply(nombre.anios, function(s) {
            output[[paste0("tbmOptimizeM_", as.character(s), "_image")]] <- renderImage(
              {
                imgfile <- ""
                if (NROW(all.graph.names) > 0) {
                  imgtmp <- all.graph.names
                  imgtmp2 <- subset(imgtmp, imgtmp$season == as.character(s))
                  if (NROW(imgtmp2) > 0) {
                    if (file.exists(imgtmp2$file)) {
                      imgfile <- imgtmp2$file
                    }
                  }
                }
                gfile <- list(src = imgfile, contentType = "image/png", width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1), alt = "No image found")
                gfile
              },
              deleteFile = FALSE
            )
          })

          optim <- memgoodness(datfile.plot,
            i.seasons = as.numeric(input$SelectMaximum),
            i.type.threshold = as.numeric(input$typethreshold),
            i.tails.threshold = as.numeric(input$ntails),
            i.type.intensity = as.numeric(input$typeintensity),
            i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
            i.tails.intensity = as.numeric(input$ntails),
            i.type.curve = as.numeric(input$typeaveragecurve),
            i.level.curve = as.numeric(input$levelaveragecurve) / 100,
            i.type.other = as.numeric(input$typeother),
            i.level.other = as.numeric(input$levelother) / 100,
            i.method = as.numeric(input$method),
            i.param = as.numeric(optimum.by.inspection.output$optimum[as.character(input$optimmethod)]),
            i.n.max = as.numeric(input$nvalues),
            i.calculation.method = "default",
            i.goodness.method = as.character(input$validation),
            i.detection.values = seq(input$paramrange[1], input$paramrange[2], by = 0.1),
            i.weeks.above = 1,
            i.graph = FALSE,
            i.min.seasons = 3
          )$results

          updateNumericInput(session, "optimparam", value = round(as.numeric(optimum.by.inspection.output$optimum[as.character(input$optimmethod)]), 1))

          fluidPage(
            fluidRow(
              column(width = 3, shinydashboard::valueBox(format(round(optim["Sensitivity"], 2), nsmall = 2), trloc("graphs.sensitivity"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow")),
              column(width = 3, shinydashboard::valueBox(format(round(optim["Specificity"], 2), nsmall = 2), trloc("graphs.specificity"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow")),
              column(width = 3, shinydashboard::valueBox(format(round(optim["Positive predictive value"], 2), nsmall = 2), trloc("graphs.ppv"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow")),
              column(width = 3, shinydashboard::valueBox(format(round(optim["Negative predictive value"], 2), nsmall = 2), trloc("graphs.npv"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow"))
            ),
            fluidRow(
              column(width = 4, shinydashboard::valueBox(format(round(optim["Percent agreement"], 2), nsmall = 2), trloc("graphs.percent"), icon = icon("fas fa-heartbeat"), width = 12, color = "aqua")),
              column(width = 4, shinydashboard::valueBox(format(round(optim["Matthews correlation coefficient"], 2), nsmall = 2), trloc("graphs.matthews"), icon = icon("fas fa-heartbeat"), width = 12, color = "aqua")),
              column(width = 4, shinydashboard::valueBox(format(round(optim["Youdens Index"], 2), nsmall = 2), trloc("graphs.youden"), icon = icon("fas fa-heartbeat"), width = 12, color = "aqua"))
            ),
            fluidRow(
              column(width = 5, shinydashboard::valueBox(format(round(input$param, 2), nsmall = 1), trloc("main.model.optimize.automatic.currentparam"), icon = icon("fas fa-heartbeat"), width = 12, color = "red")),
              column(width = 5, shinydashboard::valueBox(format(round(as.numeric(optimum.by.inspection.output$optimum[as.character(input$optimmethod)]), 2), nsmall = 1), trloc("main.model.optimize.automatic.optimumparam"), icon = icon("fas fa-heartbeat"), width = 12, color = "olive")),
              column(width = 2, popify(title = trloc("main.model.optimize.apply.label"), content = trloc("main.model.optimize.apply.hint"), placement = "rigth", actionButton("applyOptimParam", trloc("main.model.optimize.apply.label"))))
            ),
            fluidRow(
              column(width = 12, formattable::renderFormattable({
                if (!is.null(optimum.by.inspection.output$insp.data)) {
                  temp1 <- optimum.by.inspection.output$insp.data
                  temp1 <- temp1[c("value", "sensitivity", "specificity", "positive.predictive.value", "negative.predictive.value", "percent.agreement", "matthews.correlation.coefficient", "youdens.index")]
                  names(temp1) <- c("Parameter", "Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value", "Percent agreement", "Matthews correlation coefficient", "Youdens Index")
                  rownames(temp1) <- NULL
                  opt.table <- formattable::formattable(temp1, list(
                    "Sensitivity" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
                    "Specificity" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
                    "Positive predictive value" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
                    "Negative predictive value" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
                    "Percent agreement" = fixedColorBar(color = "#A5DBEB", fixedWidth = 100, alpha = 0.5),
                    "Matthews correlation coefficient" = fixedColorBar(color = "#A5DBEB", fixedWidth = 100, alpha = 0.5),
                    "Youdens Index" = fixedColorBar(color = "#A5DBEB", fixedWidth = 100, alpha = 0.5)
                  ), digits = 2, format = "f")
                  names(opt.table) <- c(trloc("main.model.optimize.automatic.parameter"), trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
                  names(attr(opt.table, "formattable")$format[[1]]) <- c(trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
                } else {
                  temp1 <- data.frame(Error = trloc("ui.columnstwo"), row.names = NULL)
                  opt.table <- formattable::formattable(temp1)
                }
                opt.table
              }))
            )
          )
        }
      }
    }
  })

  output$tbmOptimizeA <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      return(NULL)
    } else {
      tabsetPanel(
        tabPanel(trloc("main.checkdescribe.goodness.indicators"), uiOutput("tbmOptimizeASummary")),
        tabPanel(
          trloc("main.model.optimize.detailed"),
          formattable::formattableOutput("tbmOptimizeADetail"),
          fluidRow(
            column(8),
            column(
              2,
              if (zipPresent() & openxlsxPresent()) {
                downloadButton("tbmOptimizeADetail_x", "xlsx")
              } else if (!openxlsxPresent()) {
                shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
              } else if (.Platform$OS.type == "windows") {
                shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
              } else if (.Platform$OS.type == "unix") {
                shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
              }
            ),
            column(2, downloadButton("tbmOptimizeADetail_c", "csv"))
          )
        ),
        tabPanel(trloc("main.model.optimize.graphs"), plotlyOutput("tbmOptimizeAGraph", width = "auto", height = "auto"))
      )
    }
  })

  output$tbmOptimizeASummary <- renderUI({
    dataoptim <- data_optim()
    if (is.null(dataoptim)) {
      return(NULL)
    } else {
      doptim <- dataoptim$roc.data
      optim <- doptim[doptim$value == as.numeric(dataoptim$optimum[as.character(input$optimmethod)]), ]
      updateNumericInput(session, "optimparam", value = round(as.numeric(dataoptim$optimum[as.character(input$optimmethod)]), 1))
      fluidPage(
        fluidRow(
          column(width = 3, shinydashboard::valueBox(format(round(optim["sensitivity"], 2), nsmall = 2), trloc("graphs.sensitivity"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow")),
          column(width = 3, shinydashboard::valueBox(format(round(optim["specificity"], 2), nsmall = 2), trloc("graphs.specificity"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow")),
          column(width = 3, shinydashboard::valueBox(format(round(optim["positive.predictive.value"], 2), nsmall = 2), trloc("graphs.ppv"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow")),
          column(width = 3, shinydashboard::valueBox(format(round(optim["negative.predictive.value"], 2), nsmall = 2), trloc("graphs.npv"), icon = icon("fas fa-heartbeat"), width = 12, color = "yellow"))
        ),
        fluidRow(
          column(width = 4, shinydashboard::valueBox(format(round(optim["percent.agreement"], 2), nsmall = 2), trloc("graphs.percent"), icon = icon("fas fa-heartbeat"), width = 12, color = "aqua")),
          column(width = 4, shinydashboard::valueBox(format(round(optim["matthews.correlation.coefficient"], 2), nsmall = 2), trloc("graphs.matthews"), icon = icon("fas fa-heartbeat"), width = 12, color = "aqua")),
          column(width = 4, shinydashboard::valueBox(format(round(optim["youdens.index"], 2), nsmall = 2), trloc("graphs.youden"), icon = icon("fas fa-heartbeat"), width = 12, color = "aqua"))
        ),
        fluidRow(
          column(width = 5, shinydashboard::valueBox(format(round(input$param, 2), nsmall = 1), trloc("main.model.optimize.automatic.currentparam"), icon = icon("fas fa-heartbeat"), width = 12, color = "red")),
          column(width = 5, shinydashboard::valueBox(format(round(as.numeric(dataoptim$optimum[as.character(input$optimmethod)]), 2), nsmall = 1), trloc("main.model.optimize.automatic.optimumparam"), icon = icon("fas fa-heartbeat"), width = 12, color = "olive")),
          column(width = 2, popify(title = trloc("main.model.optimize.apply.label"), content = trloc("main.model.optimize.apply.hint"), placement = "rigth", actionButton("applyOptimParam", trloc("main.model.optimize.apply.label"))))
        )
      )
    }
  })

  output$tbmOptimizeADetail <- formattable::renderFormattable({
    dataoptim <- data_optim()
    if (!is.null(dataoptim)) {
      temp1 <- dataoptim$roc.data[c("value", "sensitivity", "specificity", "positive.predictive.value", "negative.predictive.value", "percent.agreement", "matthews.correlation.coefficient", "youdens.index")]
      names(temp1) <- c("Parameter", "Sensitivity", "Specificity", "Positive predictive value", "Negative predictive value", "Percent agreement", "Matthews correlation coefficient", "Youdens Index")
      rownames(temp1) <- NULL
      roca.table <- formattable::formattable(temp1, list(
        "Sensitivity" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
        "Specificity" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
        "Positive predictive value" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
        "Negative predictive value" = fixedColorBar(color = "#FFBBFF", fixedWidth = 100, alpha = 0.5),
        "Percent agreement" = fixedColorBar(color = "#A5DBEB", fixedWidth = 100, alpha = 0.5),
        "Matthews correlation coefficient" = fixedColorBar(color = "#A5DBEB", fixedWidth = 100, alpha = 0.5),
        "Youdens Index" = fixedColorBar(color = "#A5DBEB", fixedWidth = 100, alpha = 0.5)
      ), digits = 2, format = "f")
      names(roca.table) <- c(trloc("main.model.optimize.automatic.parameter"), trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
      names(attr(roca.table, "formattable")$format[[1]]) <- c(trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
    } else {
      temp1 <- data.frame(Error = trloc("ui.columnstwo"), row.names = NULL)
      roca.table <- formattable::formattable(temp1)
    }
    roca.table
  })

  output$tbmOptimizeADetail_x <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      dataoptim <- data_optim()
      if (!is.null(dataoptim)) {
        temp1 <- dataoptim$roc.data[c("value", "sensitivity", "specificity", "positive.predictive.value", "negative.predictive.value", "percent.agreement", "matthews.correlation.coefficient", "youdens.index")]
        names(temp1) <- c(trloc("main.model.optimize.automatic.parameter"), trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
        rownames(temp1) <- NULL
        exportData(
          i.data = temp1, i.file = file,
          i.sheet = substring(trloc("app.optimization"), 1, 32), i.rownames = NA, i.format = "xlsx"
        )
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$tbmOptimizeADetail_c <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      dataoptim <- data_optim()
      if (!is.null(dataoptim)) {
        temp1 <- dataoptim$roc.data[c("value", "sensitivity", "specificity", "positive.predictive.value", "negative.predictive.value", "percent.agreement", "matthews.correlation.coefficient", "youdens.index")]
        names(temp1) <- c(trloc("main.model.optimize.automatic.parameter"), trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden"))
        rownames(temp1) <- NULL
        exportData(
          i.data = temp1, i.file = file,
          i.sheet = substring(trloc("app.optimization"), 1, 32), i.rownames = NA, i.format = "csv"
        )
      }
    },
    contentType = "text/csv"
  )

  output$tbmOptimizeAGraph <- renderPlotly({
    dataoptim <- data_optim()
    if (is.null(dataoptim)) {
      z <- NULL
    } else {
      dgraf <- subset(dataoptim$roc.data, select = c("value", "sensitivity", "specificity", "positive.predictive.value", "negative.predictive.value", "percent.agreement", "matthews.correlation.coefficient", "youdens.index"))
      names(dgraf) <- c("Parameter", c(trloc("graphs.sensitivity"), trloc("graphs.specificity"), trloc("graphs.ppv"), trloc("graphs.npv"), trloc("graphs.percent"), trloc("graphs.matthews"), trloc("graphs.youden")))
      dgrafgg <- dgraf %>% tidyr::gather(Indicator, Value, -Parameter)
      dgrafgg$Indicator <- factor(dgrafgg$Indicator, levels = names(dgraf)[-1], labels = names(dgraf)[-1])

      colors.palette <- generatePalette(
        i.number.series = NCOL(dgraf) - 1,
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      axis.x.range.original <- range(dgraf$Parameter)
      axis.x.otick <- mem:::optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 10, seq(0.1, 10, 0.1), TRUE, FALSE)
      axis.x.range <- axis.x.otick$range
      axis.x.ticks <- axis.x.otick$tickmarks
      axis.x.labels <- round(axis.x.otick$tickmarks, 1)

      axis.y.range.original <- c(0, 1)
      axis.y.otick <- mem:::optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
      axis.y.range <- axis.y.otick$range + diff(range(axis.y.otick$range)) * 0.025 * c(-1, 1)
      axis.y.ticks <- axis.y.otick$tickmarks
      axis.y.labels <- round(axis.y.otick$tickmarks, 1)
      dgrafgg$Value <- round(dgrafgg$Value, 2)
      p <- ggplot(dgrafgg, aes(x = Parameter, y = Value, color = Indicator)) +
        geom_line() +
        geom_point() +
        scale_x_continuous(breaks = axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
        scale_y_continuous(breaks = axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
        scale_color_manual(values = colors.palette$colSeasons, name = trloc("graphs.indicator")) +
        labs(title = input$textMain, x = input$textX, y = input$textY) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5))

      z <- ggplotly(p, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
      # fix popup values
      for (i in seq_len(length(z$x$data))) {
        z$x$data[[i]]$text <- gsub("Parameter", trloc("main.model.optimize.automatic.parameter"), z$x$data[[i]]$text, fixed = TRUE)
        z$x$data[[i]]$text <- gsub("Value", trloc("graphs.value"), z$x$data[[i]]$text, fixed = TRUE)
        z$x$data[[i]]$text <- gsub("Indicator", trloc("graphs.indicator"), z$x$data[[i]]$text, fixed = TRUE)
      }
    }
    z
  })

  #####################################
  ### SURVEILLANCE TAB
  #####################################

  output$tbSurveillance <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      return(NULL)
    } else {
      tabsetPanel(
        tabPanel(trloc("main.checkdescribe.data"), DT::dataTableOutput("tbsData")),
        tabPanel(trloc("main.surveillance.surveillance"), uiOutput("tbsSurveillance"))
      )
    }
  })

  output$tbsData <- DT::renderDataTable(
    {
      readdata <- read_data()
      datfile <- readdata$datasetread
      if (is.null(datfile)) {
        datatoshow <- NULL
      } else if (is.null(input$SelectSurveillance)) {
        datatoshow <- NULL
      } else {
        # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
        selectedcolumns <- selectColumns(
          i.names = names(datfile),
          i.from = input$SelectSurveillance,
          i.to = input$SelectSurveillance,
          i.exclude = "",
          i.include = input$SelectSurveillance,
          i.pandemic = as.logical("TRUE"),
          i.seasons = NA
        )
        if (length(selectedcolumns) > 0) {
          datatoshow <- format(round(datfile[selectedcolumns], 2), nsmall = 2)
        } else {
          datatoshow <- data.frame(Message = "No data selected", row.names = NULL)
        }
      }
      datatoshow
    },
    options = list(scrollX = TRUE, scrollY = "600px", paging = FALSE, dom = "Bfrtip", columnDefs = list(list(targets = "_all", class = "dt-right")))
  )

  output$tbsSurveillance <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      return(NULL)
    } else if (!(input$SelectSurveillance %in% names(datfile))) {
      return(NULL)
    } else {
      tabsetPanel(
        tabPanel(trloc("main.surveillance.surveillance.week"), plotlyOutput("tbsSurveillanceWeek", width = "auto", height = "auto")),
        if (animationmethod < 4) {
          tabPanel(trloc("main.surveillance.surveillance.animated"), imageOutput("tbsSurveillanceAnimated"))
        } else {
          cat("animation package + GraphicsMagick or ImageMagic or magick package needed for this function to work. Please install it.
")
          tabPanel(trloc("main.surveillance.surveillance.animated"), tableOutput("tbsSurveillanceAnimated_nomagick"))
        },
        tabPanel(
          trloc("main.surveillance.surveillance.average"),
          plotlyOutput("tbsSurveillanceAverage", width = "auto", height = "auto"),
          fluidRow(
            column(8),
            column(
              2,
              if (zipPresent() & openxlsxPresent()) {
                downloadButton("tbsSurveillanceAverage_x", "xlsx")
              } else if (!openxlsxPresent()) {
                shiny::actionButton(inputId = "noopenxlsx", label = trloc("ui.openxlsx"), icon = icon("fas fa-file-excel"))
              } else if (.Platform$OS.type == "windows") {
                shiny::actionButton(inputId = "noziplink", label = trloc("ui.rtools"), icon = icon("fas fa-file-excel"), onclick = "window.open('https://cran.rstudio.com/bin/windows/Rtools/', '_blank')")
              } else if (.Platform$OS.type == "unix") {
                shiny::actionButton(inputId = "noziplink", label = trloc("ui.nozip"), icon = icon("fas fa-file-excel"))
              }
            ),
            column(2, downloadButton("tbsSurveillanceAverage_c", "csv"))
          )
        )
      )
    }
  })

  output$tbsSurveillanceWeek <- renderPlotly({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      zfix <- NULL
    } else if (!(input$SelectSurveillance %in% names(datfile))) {
      zfix <- NULL
    } else {
      if (is.null(input$SelectSurveillanceWeek)) {
        SurveillanceWeek <- tail(row.names(datfile), 1)
      } else if (!(input$SelectSurveillanceWeek %in% row.names(datfile))) {
        SurveillanceWeek <- tail(row.names(datfile), 1)
      } else {
        SurveillanceWeek <- input$SelectSurveillanceWeek
      }
      if (is.null(input$SelectSurveillanceForceEpidemic)) {
        force.start <- NA
      } else if (!(input$SelectSurveillanceForceEpidemic %in% row.names(datfile))) {
        force.start <- NA
      } else {
        force.start <- input$SelectSurveillanceForceEpidemic
      }
      datamodel <- data_model()
      if (!is.null(datamodel)) {
        e.thr <- datamodel$epidemic.thresholds
        i.thr <- datamodel$intensity.thresholds
      } else {
        e.thr <- NA
        i.thr <- NA
      }
      datfile.plot <- datfile[input$SelectSurveillance]
      colors.palette <- generatePalette(
        i.number.series = NA,
        i.colObservedLines = input$colObservedLines,
        i.colObservedPoints = input$colObservedPoints,
        i.colEpidemicStart = input$colEpidemicStart,
        i.colEpidemicStop = input$colEpidemicStop,
        i.colThresholds = input$colThresholds,
        i.colSeasons = input$colSeasons,
        i.colEpidemic = input$colEpidemic
      )
      p <- plotSurveillance(
        i.data = datfile.plot,
        i.week.report = SurveillanceWeek,
        i.pre.epidemic = as.logical(input$preepidemicthr),
        i.post.epidemic = as.logical(input$postepidemicthr),
        i.epidemic.thr = e.thr,
        i.intensity = as.logical(input$intensitythr),
        i.intensity.thr = i.thr,
        i.start = as.logical(input$preepidemicthr),
        i.end = as.logical(input$postepidemicthr),
        i.force.start = force.start,
        i.textMain = input$textMain,
        i.textX = input$textX,
        i.textY = input$textY,
        i.colObservedLines = colors.palette$colObservedLines,
        i.colObservedPoints = colors.palette$colObservedPoints,
        i.colEpidemicStart = colors.palette$colEpidemicStart,
        i.colEpidemicStop = colors.palette$colEpidemicStop,
        i.colThresholds = colors.palette$colThresholds,
        i.yaxis.starts.at.0 = as.logical(input$yaxis0)
      )
      if (is.null(p)) {
        zfix <- NULL
      } else {
        z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
        zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
      }
    }
    zfix
  })

  output$tbsSurveillanceAnimated <- renderImage(
    {
      readdata <- read_data()
      datfile <- readdata$datasetread
      if (is.null(datfile)) {
        outdistAnimated <- NULL
      } else if (!(input$SelectSurveillance %in% names(datfile))) {
        outdistAnimated <- NULL
      } else {
        if (is.null(input$SelectSurveillanceWeek)) {
          SurveillanceWeek <- tail(row.names(datfile), 1)
        } else if (!(input$SelectSurveillanceWeek %in% row.names(datfile))) {
          SurveillanceWeek <- tail(row.names(datfile), 1)
        } else {
          SurveillanceWeek <- input$SelectSurveillanceWeek
        }
        if (is.null(input$SelectSurveillanceForceEpidemic)) {
          force.start <- NA
        } else if (!(input$SelectSurveillanceForceEpidemic %in% row.names(datfile))) {
          force.start <- NA
        } else {
          force.start <- input$SelectSurveillanceForceEpidemic
        }
        datamodel <- data_model()
        if (!is.null(datamodel)) {
          e.thr <- datamodel$epidemic.thresholds
          i.thr <- datamodel$intensity.thresholds
        } else {
          e.thr <- NA
          i.thr <- NA
        }
        datfile.plot <- datfile[input$SelectSurveillance]
        if (as.logical(input$yaxis0)) min.y <- 0 else min.y <- 0.95 * min(datfile.plot, na.rm = TRUE)
        max.y <- 1.05 * max(datfile.plot, na.rm = TRUE)
        if (as.logical(input$preepidemicthr)) max.y <- max(max.y, e.thr[1], na.rm = TRUE)
        if (as.logical(input$postepidemicthr)) max.y <- max(max.y, e.thr[2], na.rm = TRUE)
        if (as.logical(input$intensitythr)) max.y <- max(max.y, i.thr, na.rm = TRUE)
        n.surveillance.week <- min((seq_len(NROW(datfile)))[SurveillanceWeek == rownames(datfile)])
        colors.palette <- generatePalette(
          i.number.series = NA,
          i.colObservedLines = input$colObservedLines,
          i.colObservedPoints = input$colObservedPoints,
          i.colEpidemicStart = input$colEpidemicStart,
          i.colEpidemicStop = input$colEpidemicStop,
          i.colThresholds = input$colThresholds,
          i.colSeasons = input$colSeasons,
          i.colEpidemic = input$colEpidemic
        )
        cat("animated gif> begin\n")
        cat("animated gif> creating the frames\n")
        plot.list <- list()
        for (i in seq_len(n.surveillance.week)) {
          p <- plotSurveillance(
            i.data = datfile.plot,
            i.week.report = rownames(datfile)[i],
            i.pre.epidemic = as.logical(input$preepidemicthr),
            i.post.epidemic = as.logical(input$postepidemicthr),
            i.epidemic.thr = e.thr,
            i.intensity = as.logical(input$intensitythr),
            i.intensity.thr = i.thr,
            i.range.y = c(min.y, max.y),
            i.start = as.logical(input$preepidemicthr),
            i.end = as.logical(input$postepidemicthr),
            i.force.start = force.start,
            i.textMain = input$textMain,
            i.textX = input$textX,
            i.textY = input$textY,
            i.colObservedLines = colors.palette$colObservedLines,
            i.colObservedPoints = colors.palette$colObservedPoints,
            i.colEpidemicStart = colors.palette$colEpidemicStart,
            i.colEpidemicStop = colors.palette$colEpidemicStop,
            i.colThresholds = colors.palette$colThresholds,
            i.yaxis.starts.at.0 = as.logical(input$yaxis0)
          )
          plot.list[[i]] <- p$plot
        }
        imgfilegif <- paste0(tempdir(), "/animated.gif")
        if (animationmethod == 1) {
          cat("animated gif> using animation package with GraphicsMagic\n")
          requireNamespace("animation", quietly = TRUE)
          cat(paste0("animated gif> creating\t\t", imgfilegif, "\n"))
          animation::saveGIF(for (i in seq_len(n.surveillance.week)) print(plot.list[[i]]), movie.name = imgfilegif, interval = 0.5, autobrowse = FALSE, ani.width = as.numeric(values$gwidth1), ani.height = as.numeric(values$gheight1), loop = TRUE, convert = "gm convert")
          cat(paste0("animated gif> saving\t\t", imgfilegif, "\n"))
          cat("animated gif> end\n")
        } else if (animationmethod == 2) {
          cat("animated gif> using animation package with ImageMagic\n")
          requireNamespace("animation", quietly = TRUE)
          cat(paste0("animated gif> creating\t\t", imgfilegif, "\n"))
          animation::saveGIF(for (i in seq_len(n.surveillance.week)) print(plot.list[[i]]), movie.name = imgfilegif, interval = 0.5, autobrowse = FALSE, ani.width = as.numeric(values$gwidth1), ani.height = as.numeric(values$gheight1), loop = TRUE)
          cat(paste0("animated gif> saving\t\t", imgfilegif, "\n"))
          cat("animated gif> end\n")
        } else if (animationmethod == 3) {
          cat("animated gif> using magick package\n")
          requireNamespace("magick", quietly = TRUE)
          for (i in seq_len(n.surveillance.week)) {
            imgfile <- paste(tempdir(), "/animatedplot_", i, ".png", sep = "")
            ggsave(imgfile, plot = plot.list[[i]], width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1), units = "px", dpi = 150)
            if (i == 1) imgfilem <- magick::image_read(imgfile) else imgfilem <- c(imgfilem, magick::image_read(imgfile))
            cat(paste0("animated gif> image\t", i, "/", n.surveillance.week, "\t", imgfile, "\n"))
          }
          cat(paste0("animated gif> creating\t\t", imgfilegif, "\n"))
          anim <- magick::image_animate(imgfilem, fps = 2)
          cat(paste0("animated gif> saving\t\t", imgfilegif, "\n"))
          magick::image_write(anim, path = imgfilegif)
          cat("animated gif> end\n")
        }
        outdistAnimated <- list(src = imgfilegif, contentType = "image/gif", width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1), alt = "This is alternate text")
      }
      outdistAnimated
    },
    deleteFile = TRUE
  )

  output$tbsSurveillanceAnimated_nomagick <- renderTable({
    data.show <- data.frame(var = "magick package needed for this function to work. Please install it.")
    names(data.show) <- ""
    data.show
  })

  output$tbsSurveillanceAverage <- renderPlotly({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      zfix <- NULL
    } else if (!(input$SelectSurveillance %in% names(datfile))) {
      zfix <- NULL
    } else {
      if (is.null(input$SelectSurveillanceWeek)) {
        SurveillanceWeek <- tail(row.names(datfile), 1)
      } else if (!(input$SelectSurveillanceWeek %in% row.names(datfile))) {
        SurveillanceWeek <- tail(row.names(datfile), 1)
      } else {
        SurveillanceWeek <- input$SelectSurveillanceWeek
      }
      if (is.null(input$SelectSurveillanceForceEpidemic)) {
        force.start <- NA
      } else if (!(input$SelectSurveillanceForceEpidemic %in% row.names(datfile))) {
        force.start <- NA
      } else {
        force.start <- input$SelectSurveillanceForceEpidemic
      }
      datamodel <- data_model()
      if (is.null(datamodel)) {
        zfix <- NULL
      } else {
        e.thr <- datamodel$epidemic.thresholds
        i.thr <- datamodel$intensity.thresholds
        datfile.plot <- data.frame(datfile[input$SelectSurveillance], datamodel$typ.curve)
        survweek <- (seq_len(NROW(datfile)))[SurveillanceWeek == rownames(datfile)]
        datfile.plot[-(seq_len(survweek)), 1] <- NA
        names(datfile.plot) <- c(input$SelectSurveillance, c(trloc("graphs.lowerinterval"), trloc("graphs.averagecurve"), trloc("graphs.upperinterval")))
        colors.palette <- generatePalette(
          i.number.series = 3,
          i.colObservedLines = input$colObservedLines,
          i.colObservedPoints = input$colObservedPoints,
          i.colEpidemicStart = input$colEpidemicStart,
          i.colEpidemicStop = input$colEpidemicStop,
          i.colThresholds = input$colThresholds,
          i.colSeasons = input$colSeasons,
          i.colEpidemic = input$colEpidemic
        )
        p <- plotSeasons(datfile.plot,
          i.epidemic.thr = e.thr,
          i.intensity.thr = i.thr,
          i.pre.epidemic = as.logical(input$preepidemicthr),
          i.post.epidemic = as.logical(input$postepidemicthr),
          i.intensity = as.logical(input$intensitythr),
          i.textMain = input$textMain,
          i.textX = input$textX,
          i.textY = input$textY,
          i.type.threshold = as.numeric(input$typethreshold),
          i.tails.threshold = as.numeric(input$ntails),
          i.type.intensity = as.numeric(input$typeintensity),
          i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
          i.tails.intensity = as.numeric(input$ntails),
          i.type.curve = as.numeric(input$typeaveragecurve),
          i.level.curve = as.numeric(input$levelaveragecurve) / 100,
          i.type.other = as.numeric(input$typeother),
          i.level.other = as.numeric(input$levelother) / 100,
          i.method = as.numeric(input$method),
          i.param = as.numeric(input$param),
          i.centering = as.numeric(input$centering),
          i.n.max = as.numeric(input$nvalues),
          i.colObservedPoints = colors.palette$colObservedPoints,
          i.colSeasons = c(colors.palette$colObservedLines, colors.palette$colSeasons[c(3, 2, 3)]),
          i.colThresholds = colors.palette$colThresholds,
          i.yaxis.starts.at.0 = as.logical(input$yaxis0),
          i.use.t = as.logical(input$usetdistribution)
        )
        if (is.null(p)) {
          zfix <- NULL
        } else {
          z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
          zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
        }
      }
    }
    zfix
  })

  output$tbsSurveillanceAverage_x <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".xlsx", sep = "")
    },
    content = function(file) {
      readdata <- read_data()
      datfile <- readdata$datasetread
      if (!is.null(datfile)) {
        if (input$SelectSurveillance %in% names(datfile)) {
          if (is.null(input$SelectSurveillanceWeek)) {
            SurveillanceWeek <- tail(row.names(datfile), 1)
          } else if (!(input$SelectSurveillanceWeek %in% row.names(datfile))) {
            SurveillanceWeek <- tail(row.names(datfile), 1)
          } else {
            SurveillanceWeek <- input$SelectSurveillanceWeek
          }
          if (is.null(input$SelectSurveillanceForceEpidemic)) {
            force.start <- NA
          } else if (!(input$SelectSurveillanceForceEpidemic %in% row.names(datfile))) {
            force.start <- NA
          } else {
            force.start <- input$SelectSurveillanceForceEpidemic
          }
          datamodel <- data_model()
          if (!is.null(datamodel)) {
            e.thr <- datamodel$epidemic.thresholds
            i.thr <- datamodel$intensity.thresholds
            datfile.plot <- data.frame(datfile[input$SelectSurveillance], datamodel$typ.curve)
            survweek <- (seq_len(NROW(datfile)))[SurveillanceWeek == rownames(datfile)]
            datfile.plot[-(seq_len(survweek)), 1] <- NA
            names(datfile.plot) <- c(input$SelectSurveillance, c(trloc("graphs.lowerinterval"), trloc("graphs.averagecurve"), trloc("graphs.upperinterval")))
            colors.palette <- generatePalette(
              i.number.series = 3,
              i.colObservedLines = input$colObservedLines,
              i.colObservedPoints = input$colObservedPoints,
              i.colEpidemicStart = input$colEpidemicStart,
              i.colEpidemicStop = input$colEpidemicStop,
              i.colThresholds = input$colThresholds,
              i.colSeasons = input$colSeasons,
              i.colEpidemic = input$colEpidemic
            )
            p <- plotSeasons(datfile.plot,
              i.epidemic.thr = e.thr,
              i.intensity.thr = i.thr,
              i.pre.epidemic = as.logical(input$preepidemicthr),
              i.post.epidemic = as.logical(input$postepidemicthr),
              i.intensity = as.logical(input$intensitythr),
              i.textMain = input$textMain,
              i.textX = input$textX,
              i.textY = input$textY,
              i.type.threshold = as.numeric(input$typethreshold),
              i.tails.threshold = as.numeric(input$ntails),
              i.type.intensity = as.numeric(input$typeintensity),
              i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
              i.tails.intensity = as.numeric(input$ntails),
              i.type.curve = as.numeric(input$typeaveragecurve),
              i.level.curve = as.numeric(input$levelaveragecurve) / 100,
              i.type.other = as.numeric(input$typeother),
              i.level.other = as.numeric(input$levelother) / 100,
              i.method = as.numeric(input$method),
              i.param = as.numeric(input$param),
              i.centering = as.numeric(input$centering),
              i.n.max = as.numeric(input$nvalues),
              i.colObservedPoints = colors.palette$colObservedPoints,
              i.colSeasons = c(colors.palette$colObservedLines, colors.palette$colSeasons[c(3, 2, 3)]),
              i.colThresholds = colors.palette$colThresholds,
              i.yaxis.starts.at.0 = as.logical(input$yaxis0),
              i.use.t = as.logical(input$usetdistribution)
            )
            if (!is.null(p)) {
              temp1 <- p$gdata
              temp2 <- temp1 %>%
                select(variable, week, value) %>%
                tidyr::spread(variable, value, drop = FALSE, fill = NA)

              temp2 <- temp2[order(temp2$week), p$labels]
              row.names(temp2) <- p$weeklabels
              temp2$week <- NULL
              names(temp2)[names(temp2) == "Week no"] <- trloc("app.weekno")
              names(temp2)[names(temp2) == "Lower interval"] <- trloc("graphs.lowerinterval")
              names(temp2)[names(temp2) == "Average curve"] <- trloc("graphs.averagecurve")
              names(temp2)[names(temp2) == "Upper interval"] <- trloc("graphs.upperinterval")
              names(temp2)[names(temp2) == "Epidemic thr."] <- trloc("graphs.prethreshold.short")
              names(temp2)[names(temp2) == "Medium thr."] <- trloc("graphs.mediumthreshold.short")
              names(temp2)[names(temp2) == "High thr."] <- trloc("graphs.highthreshold.short")
              names(temp2)[names(temp2) == "Very high thr."] <- trloc("graphs.veryhighthreshold.short")
              exportData(
                i.data = temp2, i.file = file,
                i.sheet = substring(trloc("graphs.averagecurve"), 1, 32), i.rownames = trloc("app.weekno"), i.format = "xlsx"
              )
            }
          }
        }
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$tbsSurveillanceAverage_c <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      readdata <- read_data()
      datfile <- readdata$datasetread
      if (!is.null(datfile)) {
        if (input$SelectSurveillance %in% names(datfile)) {
          if (is.null(input$SelectSurveillanceWeek)) {
            SurveillanceWeek <- tail(row.names(datfile), 1)
          } else if (!(input$SelectSurveillanceWeek %in% row.names(datfile))) {
            SurveillanceWeek <- tail(row.names(datfile), 1)
          } else {
            SurveillanceWeek <- input$SelectSurveillanceWeek
          }
          if (is.null(input$SelectSurveillanceForceEpidemic)) {
            force.start <- NA
          } else if (!(input$SelectSurveillanceForceEpidemic %in% row.names(datfile))) {
            force.start <- NA
          } else {
            force.start <- input$SelectSurveillanceForceEpidemic
          }
          datamodel <- data_model()
          if (!is.null(datamodel)) {
            e.thr <- datamodel$epidemic.thresholds
            i.thr <- datamodel$intensity.thresholds
            datfile.plot <- data.frame(datfile[input$SelectSurveillance], datamodel$typ.curve)
            survweek <- (seq_len(NROW(datfile)))[SurveillanceWeek == rownames(datfile)]
            datfile.plot[-(seq_len(survweek)), 1] <- NA
            names(datfile.plot) <- c(input$SelectSurveillance, c(trloc("graphs.lowerinterval"), trloc("graphs.averagecurve"), trloc("graphs.upperinterval")))
            colors.palette <- generatePalette(
              i.number.series = 3,
              i.colObservedLines = input$colObservedLines,
              i.colObservedPoints = input$colObservedPoints,
              i.colEpidemicStart = input$colEpidemicStart,
              i.colEpidemicStop = input$colEpidemicStop,
              i.colThresholds = input$colThresholds,
              i.colSeasons = input$colSeasons,
              i.colEpidemic = input$colEpidemic
            )
            p <- plotSeasons(datfile.plot,
              i.epidemic.thr = e.thr,
              i.intensity.thr = i.thr,
              i.pre.epidemic = as.logical(input$preepidemicthr),
              i.post.epidemic = as.logical(input$postepidemicthr),
              i.intensity = as.logical(input$intensitythr),
              i.textMain = input$textMain,
              i.textX = input$textX,
              i.textY = input$textY,
              i.type.threshold = as.numeric(input$typethreshold),
              i.tails.threshold = as.numeric(input$ntails),
              i.type.intensity = as.numeric(input$typeintensity),
              i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
              i.tails.intensity = as.numeric(input$ntails),
              i.type.curve = as.numeric(input$typeaveragecurve),
              i.level.curve = as.numeric(input$levelaveragecurve) / 100,
              i.type.other = as.numeric(input$typeother),
              i.level.other = as.numeric(input$levelother) / 100,
              i.method = as.numeric(input$method),
              i.param = as.numeric(input$param),
              i.centering = as.numeric(input$centering),
              i.n.max = as.numeric(input$nvalues),
              i.colObservedPoints = colors.palette$colObservedPoints,
              i.colSeasons = c(colors.palette$colObservedLines, colors.palette$colSeasons[c(3, 2, 3)]),
              i.colThresholds = colors.palette$colThresholds,
              i.yaxis.starts.at.0 = as.logical(input$yaxis0),
              i.use.t = as.logical(input$usetdistribution)
            )
            if (!is.null(p)) {
              temp1 <- p$gdata
              temp2 <- temp1 %>%
                select(variable, week, value) %>%
                tidyr::spread(variable, value, drop = FALSE, fill = NA)
              temp2 <- temp2[order(temp2$week), p$labels]
              row.names(temp2) <- p$weeklabels
              temp2$week <- NULL
              names(temp2)[names(temp2) == "Week no"] <- trloc("app.weekno")
              names(temp2)[names(temp2) == "Lower interval"] <- trloc("graphs.lowerinterval")
              names(temp2)[names(temp2) == "Average curve"] <- trloc("graphs.averagecurve")
              names(temp2)[names(temp2) == "Upper interval"] <- trloc("graphs.upperinterval")
              names(temp2)[names(temp2) == "Epidemic thr."] <- trloc("graphs.prethreshold.short")
              names(temp2)[names(temp2) == "Medium thr."] <- trloc("graphs.mediumthreshold.short")
              names(temp2)[names(temp2) == "High thr."] <- trloc("graphs.highthreshold.short")
              names(temp2)[names(temp2) == "Very high thr."] <- trloc("graphs.veryhighthreshold.short")
              exportData(
                i.data = temp2, i.file = file,
                i.sheet = substring(trloc("graphs.averagecurve"), 1, 32), i.rownames = trloc("app.weekno"), i.format = "csv"
              )
            }
          }
        }
      }
    },
    contentType = "text/csv"
  )

  #####################################
  ### VISUALIZE TAB
  #####################################

  output$tbVisualize <- renderUI({
    readdata <- read_data()
    datfile <- readdata$datasetread
    if (is.null(datfile)) {
      return(NULL)
    } else if (is.null(input$SelectSeasons)) {
      return(NULL)
    } else {
      toinclude <- input$SelectSeasons
      selectedcolumns <- selectColumns(
        i.names = names(datfile),
        i.from = input$SelectSeasons[1],
        i.to = input$SelectSeasons[1],
        i.exclude = "",
        i.include = toinclude,
        i.pandemic = as.logical("TRUE"),
        i.seasons = NA
      )
      if (length(selectedcolumns) > 0) {
        tabsetPanel(
          tabPanel(trloc("main.checkdescribe.data"), DT::dataTableOutput("tbvData")),
          tabPanel(trloc("main.checkdescribe.seasons"), plotlyOutput("tbvSeasons", width = "auto", height = "auto")),
          tabPanel(trloc("main.checkdescribe.series"), plotlyOutput("tbvSeries", width = "auto", height = "auto")),
          tabPanel(trloc("main.checkdescribe.timing"), uiOutput("tbvTiming"))
        )
      } else {
        return(NULL)
      }
    }
  })

  output$tbvData <- DT::renderDataTable(
    {
      readdata <- read_data()
      datfile <- readdata$datasetread
      if (is.null(datfile)) {
        datatoshow <- NULL
      } else if (is.null(input$SelectSeasons)) {
        datatoshow <- NULL
      } else {
        # Shows the data that's going to be used for mem calculations, plus the seasons to be added to the graph and surveillance
        toinclude <- input$SelectSeasons
        selectedcolumns <- selectColumns(
          i.names = names(datfile),
          i.from = input$SelectSeasons[1],
          i.to = input$SelectSeasons[1],
          i.exclude = "",
          i.include = toinclude,
          i.pandemic = as.logical("TRUE"),
          i.seasons = NA
        )
        if (length(selectedcolumns) > 0) {
          datatoshow <- format(round(datfile[selectedcolumns], 2), nsmall = 2)
        } else {
          datatoshow <- data.frame(Message = "No data selected", row.names = NULL)
        }
      }
      datatoshow
    },
    options = list(scrollX = TRUE, scrollY = "600px", paging = FALSE, dom = "Bfrtip", columnDefs = list(list(targets = "_all", class = "dt-right")))
  )

  output$tbvSeasons <- renderPlotly({
    readdata <- read_data()
    datfile <- readdata$datasetread
    datamodel <- data_model()
    if (is.null(datfile)) {
      zfix <- NULL
    } else if (is.null(datamodel)) {
      zfix <- NULL
    } else if (is.null(input$SelectSeasons)) {
      zfix <- NULL
    } else {
      e.thr <- datamodel$epidemic.thresholds
      i.thr <- datamodel$intensity.thresholds
      toinclude <- input$SelectSeasons
      selectedcolumns <- selectColumns(
        i.names = names(datfile),
        i.from = input$SelectSeasons[1],
        i.to = input$SelectSeasons[1],
        i.exclude = "",
        i.include = toinclude,
        i.pandemic = as.logical("TRUE"),
        i.seasons = NA
      )
      if (length(selectedcolumns) == 0) {
        zfix <- NULL
      } else {
        datfile.plot <- datfile[selectedcolumns]
        colors.palette <- generatePalette(
          i.number.series = NCOL(datfile.plot),
          i.colObservedLines = input$colObservedLines,
          i.colObservedPoints = input$colObservedPoints,
          i.colEpidemicStart = input$colEpidemicStart,
          i.colEpidemicStop = input$colEpidemicStop,
          i.colThresholds = input$colThresholds,
          i.colSeasons = input$colSeasons,
          i.colEpidemic = input$colEpidemic
        )
        p <- plotSeasons(datfile.plot,
          i.epidemic.thr = e.thr,
          i.intensity.thr = i.thr,
          i.pre.epidemic = as.logical(input$preepidemicthr),
          i.post.epidemic = as.logical(input$postepidemicthr),
          i.intensity = as.logical(input$intensitythr),
          i.textMain = input$textMain,
          i.textX = input$textX,
          i.textY = input$textY,
          i.type.threshold = as.numeric(input$typethreshold),
          i.tails.threshold = as.numeric(input$ntails),
          i.type.intensity = as.numeric(input$typeintensity),
          i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
          i.tails.intensity = as.numeric(input$ntails),
          i.type.curve = as.numeric(input$typeaveragecurve),
          i.level.curve = as.numeric(input$levelaveragecurve) / 100,
          i.type.other = as.numeric(input$typeother),
          i.level.other = as.numeric(input$levelother) / 100,
          i.method = as.numeric(input$method),
          i.param = as.numeric(input$param),
          i.centering = as.numeric(input$centering),
          i.n.max = as.numeric(input$nvalues),
          i.colObservedPoints = colors.palette$colObservedPoints,
          i.colSeasons = colors.palette$colSeasons,
          i.colThresholds = colors.palette$colThresholds,
          i.yaxis.starts.at.0 = as.logical(input$yaxis0),
          i.use.t = as.logical(input$usetdistribution)
        )
        if (is.null(p)) {
          zfix <- NULL
        } else {
          z <- ggplotly(p$plot, width = as.numeric(values$gwidth1), height = as.numeric(values$gheight1))
          zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
        }
      }
    }
    zfix
  })

  output$tbvSeries <- renderPlotly({
    readdata <- read_data()
    datfile <- readdata$datasetread
    datamodel <- data_model()
    if (is.null(datfile)) {
      zfix <- NULL
    } else if (is.null(datamodel)) {
      zfix <- NULL
    } else if (is.null(input$SelectSeasons)) {
      zfix <- NULL
    } else {
      e.thr <- datamodel$epidemic.thresholds
      i.thr <- datamodel$intensity.thresholds
      toinclude <- input$SelectSeasons
      selectedcolumns <- selectColumns(
        i.names = names(datfile),
        i.from = input$SelectSeasons[1],
        i.to = input$SelectSeasons[1],
        i.exclude = "",
        i.include = toinclude,
        i.pandemic = as.logical("TRUE"),
        i.seasons = NA
      )
      if (length(selectedcolumns) == 0) {
        zfix <- NULL
      } else {
        datfile.plot <- datfile[selectedcolumns]
        colors.palette <- generatePalette(
          i.number.series = NA,
          i.colObservedLines = input$colObservedLines,
          i.colObservedPoints = input$colObservedPoints,
          i.colEpidemicStart = input$colEpidemicStart,
          i.colEpidemicStop = input$colEpidemicStop,
          i.colThresholds = input$colThresholds,
          i.colSeasons = input$colSeasons,
          i.colEpidemic = input$colEpidemic
        )
        p <- plotSeries(
          i.data = datfile.plot,
          i.plot.timing = as.logical(input$plottiming),
          i.range.x = NA,
          i.pre.epidemic = as.logical(input$preepidemicthr),
          i.post.epidemic = as.logical(input$postepidemicthr),
          i.epidemic.thr = e.thr,
          i.intensity = as.logical(input$intensitythr),
          i.intensity.thr = i.thr,
          i.range.y = NA,
          i.replace.x.cr = TRUE,
          i.textMain = input$textMain,
          i.textX = input$textX,
          i.textY = input$textY,
          i.type.threshold = as.numeric(input$typethreshold),
          i.tails.threshold = as.numeric(input$ntails),
          i.type.intensity = as.numeric(input$typeintensity),
          i.level.intensity = as.numeric(c(input$levelintensitym, input$levelintensityh, input$levelintensityv)) / 100,
          i.tails.intensity = as.numeric(input$ntails),
          i.type.curve = as.numeric(input$typeaveragecurve),
          i.level.curve = as.numeric(input$levelaveragecurve) / 100,
          i.type.other = as.numeric(input$typeother),
          i.level.other = as.numeric(input$levelother) / 100,
          i.method = as.numeric(input$method),
          i.param = as.numeric(input$param),
          i.centering = as.numeric(input$centering),
          i.n.max = as.numeric(input$nvalues),
          i.colObservedLines = colors.palette$colObservedLines,
          i.colThresholds = colors.palette$colThresholds,
          i.colObservedPoints = colors.palette$colObservedPoints,
          i.colEpidemic = colors.palette$colEpidemic,
          i.yaxis.starts.at.0 = as.logical(input$yaxis0),
          i.use.t = as.logical(input$usetdistribution)
        )
        if (is.null(p)) {
          zfix <- NULL
        } else {
          z <- ggplotly(p$plot, width = as.numeric(values$gwidth2), height = as.numeric(values$gheight2))
          zfix <- fixPlotly(z, p$labels, p$haslines, p$haspoints, trloc("graphs.week"), "value", p$weeklabels)
        }
      }
    }
    zfix
  })

  output$tbvTiming <- renderUI({
    tabnames <- input$SelectSeasons
    if (is.null(tabnames)) {
      return(NULL)
    } else {
      do.call(
        tabsetPanel,
        ## Create a set of tabPanel functions dependent on tabnames
        lapply(tabnames, function(s) {
          ## Populate the tabPanel with a dataTableOutput layout, with ID specific to the sample.
          ## Can also accommodate additional layout parts by adding additional call() to call("tabPanel")
          call("tabPanel", s, call("uiOutput", outputId = paste0("tbvTiming_", s), width = "auto", height = "auto"))
        })
      )
    }
  })

  #####################################
  ### UI STRUCTURE
  #####################################

  output$uifile <- renderUI({
    popify(
      fileInput("file", label = h4(trloc("selections.loadfile"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), accept = c("csv", "dat", "prn", "txt", "xls", "xlsx", "mdb", "accdb", "rdata")),
      title = trloc("selections.loadfile"), content = trloc("ui.memreads"), placement = "right", trigger = "focus", options = list(container = "body")
    )
  })

  output$uiDataset <- renderUI({
    shinydashboard::box(
      title = trloc("selections.dataset.dataset.label"), status = "warning", solidHeader = FALSE, width = 12, background = "navy", collapsible = FALSE, collapsed = FALSE,
      fluidRow(
        column(12, offset = 0, style = "padding:0px;", uiOutput("uidata"))
      ),
      fluidRow(
        column(6, offset = 0, style = "padding:0px;", uiOutput("uifirstWeek")),
        column(6, offset = 0, style = "padding:0px;", uiOutput("uilastWeek"))
      ),
      fluidRow(
        column(12, offset = 0, style = "padding:0px;", uiOutput("uiprocess"))
      ),
      uiOutput("uitransformation"),
      uiOutput("uiwaves")
    )
  })

  output$uidata <- renderUI({
    popify(
      selectInput("dataset", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset")), size = 1, selectize = FALSE, choices = getDatasets(), selected = NULL),
      title = trloc("selections.dataset"), content = trloc("selections.dataset.dataset.hint"), placement = "right", trigger = "focus", options = list(container = "body")
    )
  })

  output$uifirstWeek <- renderUI({
    popify(
      selectInput("firstWeek", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset.firstweek.label")), size = 1, selectize = FALSE, choices = getWeeksOriginal(), selected = head(getWeeksOriginal(), 1)),
      title = trloc("selections.dataset.firstweek.label"), content = trloc("selections.dataset.firstweek.hint"), placement = "right", trigger = "focus", options = list(container = "body")
    )
  })

  output$uilastWeek <- renderUI({
    popify(
      selectInput("lastWeek", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset.lastweek.label")), size = 1, selectize = FALSE, choices = getWeeksOriginal(), selected = tail(getWeeksOriginal(), 1)),
      title = trloc("selections.dataset.lastweek.label"), content = trloc("selections.dataset.lastweek.hint"), placement = "right", trigger = "focus", options = list(container = "body")
    )
  })

  output$uitransformation <- renderUI({
    transformation.list <- list("No transformation" = 1, "Odd" = 2, "Fill missings" = 3, "Smoothing regression" = 4, "Loess" = 5, "Spline" = 6, "Moving average" = 7)
    names(transformation.list) <- c(trloc("selections.dataset.transformation.no"), trloc("selections.dataset.transformation.odd"), trloc("selections.dataset.transformation.fillmiss"), trloc("selections.dataset.transformation.smothing"), trloc("selections.dataset.transformation.loess"), trloc("selections.dataset.transformation.spline"), trloc("selections.dataset.transformation.movingaverage"))
    fluidRow(
      conditionalPanel(
        condition = "input.processdata",
        popify(
          selectInput("transformation", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset.transformation.label")), size = 1, selectize = FALSE, choices = transformation.list, selected = default_values$transformation),
          title = trloc("selections.dataset.transformation.label"), content = trloc("selections.dataset.transformation.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        )
      ),
      conditionalPanel(
        condition = "input.transformation == 5 & input.advanced & input.processdata",
        popify(
          sliderInput("loesspan", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset.transformation.loess.span.label")), min = default_values$loesspan$min, max = default_values$loesspan$max, value = default_values$loesspan$value, step = default_values$loesspan$step),
          title = trloc("selections.dataset.transformation.loess.span.label"), content = trloc("selections.dataset.transformation.loess.span.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        )
      ),
      conditionalPanel(
        condition = "input.transformation == 7 & input.advanced & input.processdata",
        popify(
          sliderInput("movavgweeks", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset.transformation.movingaverage.weeks.label")), min = default_values$movavgweeks$min, max = default_values$movavgweeks$max, value = default_values$movavgweeks$value, step = default_values$movavgweeks$step),
          title = trloc("selections.dataset.transformation.movingaverage.weeks.label"), content = trloc("selections.dataset.transformation.movingaverage.weeks.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        )
      ),
      conditionalPanel(
        condition = "input.transformation == 4 & input.advanced & input.processdata",
        popify(
          shinyWidgets::prettyCheckbox(
            inputId = "smregressionoptimum",
            label = trloc("selections.dataset.transformation.smothing.optimum.label"),
            value = default_values$smregressionoptimum,
            shape = "curve"
          ),
          title = trloc("selections.dataset.transformation.smothing.optimum.label"), content = trloc("selections.dataset.transformation.smothing.optimum.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        )
      ),
      conditionalPanel(
        condition = "input.transformation == 4 & input.advanced & !input.smregressionoptimum & input.processdata",
        popify(
          sliderInput("smregressionsmoothing", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset.transformation.smothing.parameter.label")), min = default_values$smregressionsmoothing$min, max = default_values$smregressionsmoothing$max, value = default_values$smregressionsmoothing$value, step = default_values$smregressionsmoothing$step),
          title = trloc("selections.dataset.transformation.smothing.parameter.label"), content = trloc("selections.dataset.transformation.smothing.parameter.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        )
      ),
      conditionalPanel(
        condition = "(input.transformation == 5 | input.transformation == 6) & input.advanced & input.processdata",
        popify(
          shinyWidgets::prettyCheckbox(
            inputId = "transfpositive",
            label = trloc("selections.dataset.transformation.loess.positive.label"),
            value = default_values$transfpositive,
            shape = "curve"
          ),
          title = trloc("selections.dataset.transformation.loess.positive.label"), content = trloc("selections.dataset.transformation.loess.positive.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        )
      )
    )
  })

  output$uiwaves <- renderUI({
    if (as.logical(input$experimental)) {
      waves.list <- list("One wave/season" = 1, "Two waves/season (observed)" = 2, "Two waves/season (expected)" = 3, "Multiple waves/series" = 4)
      names(waves.list) <- c(trloc("selections.dataset.wavesdetection.one"), trloc("selections.dataset.wavesdetection.twoobserved"), trloc("selections.dataset.wavesdetection.twoexpected"), trloc("selections.dataset.wavesdetection.multiple"))
    } else {
      waves.list <- list("One wave/season" = 1, "Two waves/season (observed)" = 2, "Two waves/season (expected)" = 3)
      names(waves.list) <- c(trloc("selections.dataset.wavesdetection.one"), trloc("selections.dataset.wavesdetection.twoobserved"), trloc("selections.dataset.wavesdetection.twoexpected"))
    }
    fluidRow(
      conditionalPanel(
        condition = "input.processdata",
        popify(
          selectInput("waves", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset.wavesdetection.label")), size = 1, selectize = FALSE, choices = waves.list, selected = default_values$waves),
          title = trloc("selections.dataset.wavesdetection.label"), content = trloc("selections.dataset.wavesdetection.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        )
      ),
      conditionalPanel(
        condition = "(input.waves == 2 | input.waves == 3) & input.advanced & input.processdata",
        popify(
          sliderInput("twowavesproportion", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset.wavesdetection.twoobserved.minimumprop.label")), min = default_values$twowavesproportion$min, max = default_values$twowavesproportion$max, value = default_values$twowavesproportion$value, step = default_values$twowavesproportion$step),
          title = trloc("selections.dataset.wavesdetection.twoobserved.minimumprop.label"), content = trloc("selections.dataset.wavesdetection.twoobserved.minimumprop.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        )
      ),
      conditionalPanel(
        condition = "input.waves == 4 & input.experimental & input.advanced & input.processdata",
        fluidRow(
          column(
            6,
            popify(
              numericInput("numberwaves", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset.wavesdetection.multiple.no.label")), value = default_values$numberwaves$value, min = default_values$numberwaves$min, max = default_values$numberwaves$max, step = default_values$numberwaves$step),
              title = trloc("selections.dataset.wavesdetection.multiple.no.label"), content = trloc("selections.dataset.wavesdetection.multiple.no.hint"), placement = "right", trigger = "focus", options = list(container = "body")
            )
          ),
          column(
            6,
            popify(
              numericInput("wavesseparation", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset.wavesdetection.multiple.separation.label")), value = default_values$wavesseparation$value, min = default_values$wavesseparation$min, max = default_values$wavesseparation$max, step = default_values$wavesseparation$step),
              title = trloc("selections.dataset.wavesdetection.multiple.separation.label"), content = trloc("selections.dataset.wavesdetection.multiple.separation.hint"), placement = "right", trigger = "focus", options = list(container = "body")
            )
          )
        ),
        fluidRow(
          column(
            6,
            popify(
              numericInput("wavesparam1", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset.wavesdetection.multiple.param1.label")), value = default_values$wavesparam1$value, min = default_values$wavesparam1$min, max = default_values$wavesparam1$max, step = default_values$wavesparam1$step),
              title = trloc("selections.dataset.wavesdetection.multiple.param1.label"), content = trloc("selections.dataset.wavesdetection.multiple.param1.hint"), placement = "right", trigger = "focus", options = list(container = "body")
            )
          ),
          column(
            6,
            popify(
              numericInput("wavesparam2", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.dataset.wavesdetection.multiple.param2.label")), value = default_values$wavesparam2$value, min = default_values$wavesparam2$min, max = default_values$wavesparam2$max, step = default_values$wavesparam2$step),
              title = trloc("selections.dataset.wavesdetection.multiple.param2.label"), content = trloc("selections.dataset.wavesdetection.multiple.param2.hint"), placement = "right", trigger = "focus", options = list(container = "body")
            )
          )
        )
      )
    )
  })

  output$uiprocess <- renderUI({
    popify(
      shinyWidgets::materialSwitch(
        inputId = "processdata",
        label = trloc("selections.dataset.processdata.label"),
        value = default_values$processdata,
        right = TRUE,
        status = "info"
      ),
      title = trloc("selections.dataset.processdata.label"), content = trloc("selections.dataset.processdata.hint"), placement = "right", trigger = "focus", options = list(container = "body")
    )
  })

  output$uiModel <- renderUI({
    shinydashboard::box(
      title = trloc("selections.model"), status = "primary", solidHeader = TRUE, width = 12, background = "black", collapsible = TRUE, collapsed = TRUE,
      fluidRow(
        column(6, offset = 0, style = "padding:0px;", popify(
          selectInput("SelectFrom", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.model.from.label")), size = 1, selectize = FALSE, choices = getSeasons(), selected = head(getSeasons(), 1)),
          title = trloc("selections.model.from.label"), content = trloc("selections.model.from.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        )),
        column(6, offset = 0, style = "padding:0px;", popify(
          selectInput("SelectTo", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.model.to.label")), size = 1, selectize = FALSE, choices = getSeasons(), selected = tail(getSeasons(), 2)[1]),
          title = trloc("selections.model.to.label"), content = trloc("selections.model.to.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        ))
      ),
      fluidRow(
        column(6,
          offset = 0, style = "padding:0px;",
          popify(
            shinyWidgets::pickerInput(
              inputId = "SelectExclude",
              label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.model.exclude.label")),
              multiple = TRUE,
              choices = getSeasons(),
              selected = NULL
            ),
            title = trloc("selections.model.exclude.label"), content = trloc("selections.model.exclude.hint"), placement = "right", trigger = "focus", options = list(container = "body")
          )
        ),
        column(6, offset = 0, style = "padding:0px;", popify(
          numericInput("SelectMaximum", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.model.seasons.label")), 10, min = 2, max = NA, step = 1),
          title = trloc("selections.model.seasons.label"), content = trloc("selections.model.seasons.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        ))
      )
    )
  })

  output$uiSurveillance <- renderUI({
    shinydashboard::box(
      title = trloc("selections.surveillance"), status = "primary", solidHeader = TRUE, width = 12, background = "black", collapsible = TRUE, collapsed = TRUE,
      fluidRow(
        column(12, offset = 0, style = "padding:0px;", popify(
          selectInput("SelectSurveillance", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.surveillance.season.label")), size = 1, selectize = FALSE, choices = getSeasons(), selected = tail(getSeasons(), 1)),
          title = trloc("selections.surveillance.season.label"), content = trloc("selections.surveillance.season.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        ))
      ),
      fluidRow(
        column(6, offset = 0, style = "padding:0px;", popify(
          selectInput("SelectSurveillanceWeek", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.surveillance.week.label")), size = 1, selectize = FALSE, choices = getWeeksFiltered(), selected = tail(getWeeksFiltered(), 1)),
          title = trloc("selections.surveillance.week.label"), content = trloc("selections.surveillance.week.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        )),
        column(6, offset = 0, style = "padding:0px;", popify(
          selectInput("SelectSurveillanceForceEpidemic", h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("selections.surveillance.force.label")), size = 1, selectize = FALSE, choices = c("", getWeeksFiltered()), select = ""),
          title = trloc("selections.surveillance.force.label"), content = trloc("selections.surveillance.force.hint"), placement = "right", trigger = "focus", options = list(container = "body")
        ))
      )
    )
  })

  output$uiVisualize <- renderUI({
    shinydashboard::box(
      title = trloc("selections.visualize.label"), status = "primary", solidHeader = TRUE, width = 12, background = "black", collapsible = TRUE, collapsed = TRUE,
      popify(
        shinyWidgets::pickerInput(
          inputId = "SelectSeasons",
          label = h5(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("main.checkdescribe.seasons")),
          choices = getSeasons(), multiple = TRUE, selected = NULL
        ),
        title = trloc("main.checkdescribe.seasons"), content = trloc("selections.visualize.hint"), placement = "right", trigger = "focus", options = list(container = "body")
      )
    )
  })

  output$uiThresholds <- renderUI({
    shinydashboard::box(
      title = trloc("selections.thresholds"), status = "primary", solidHeader = TRUE, width = 12, background = "black", collapsible = TRUE, collapsed = TRUE,
      popify(
        shinyWidgets::prettyCheckbox(
          inputId = "preepidemicthr",
          label = trloc("selections.thresholds.pre.label"),
          value = default_values$preepidemicthr,
          shape = "curve"
        ),
        title = trloc("selections.thresholds.pre.label"), content = trloc("selections.thresholds.pre.hint"), placement = "right", trigger = "focus", options = list(container = "body")
      ),
      popify(
        shinyWidgets::prettyCheckbox(
          inputId = "postepidemicthr",
          label = trloc("selections.thresholds.post.label"),
          value = default_values$postepidemicthr,
          shape = "curve"
        ),
        title = trloc("selections.thresholds.post.label"), content = trloc("selections.thresholds.post.hint"), placement = "right", trigger = "focus", options = list(container = "body")
      ),
      popify(
        shinyWidgets::prettyCheckbox(
          inputId = "intensitythr",
          label = trloc("selections.thresholds.intensity.label"),
          value = default_values$intensitythr,
          shape = "curve"
        ),
        title = trloc("selections.thresholds.intensity.label"), content = trloc("selections.thresholds.intensity.hint"), placement = "right", trigger = "focus", options = list(container = "body")
      ),
      popify(
        shinyWidgets::prettyCheckbox(
          inputId = "plottiming",
          label = trloc("selections.timing.label"),
          value = default_values$intensitythr,
          shape = "curve"
        ),
        title = trloc("selections.timing.label"), content = trloc("selections.timing.hint"), placement = "right", trigger = "focus", options = list(container = "body")
      )
    )
  })

  output$uiTitle <- renderUI({
    fluidPage(
      tagList(
        singleton(tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "busyIndicator.css")
        )),
        div(class = "shinysky-busy-indicator", p(trloc("ui.calculating")), img(src = "ajaxloaderq.gif")),
        tags$script(sprintf(
          "	setInterval(function(){
          if ($('html').hasClass('shiny-busy')) {
          setTimeout(function() {
          if ($('html').hasClass('shiny-busy')) {
          $('div.shinysky-busy-indicator').show()
          }
          }, %d)
          } else {
          $('div.shinysky-busy-indicator').hide()
          }
          },100)
          ", 500
        ))
      ),
      titlePanel(h1(trloc("ui.moving")))
    )
  })

  output$uiProcedures <- renderUI({
    tabBox(
      title = h3(trloc("main.procedures"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), width = 12, height = "auto",
      tabPanel(h4(trloc("main.checkdescribe.label"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), trloc("main.checkdescribe.hint"), uiOutput("tbData")),
      tabPanel(h4(trloc("main.model.label"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), trloc("main.model.hint"), uiOutput("tbModel")),
      tabPanel(h4(trloc("main.surveillance.label"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), trloc("main.surveillance.hint"), uiOutput("tbSurveillance")),
      tabPanel(h4(trloc("main.visualize.label"), tags$style(type = "text/css", "#q1 {font-weight: bold;}")), trloc("main.visualize.hint"), uiOutput("tbVisualize"))
    )
  })

  output$uiTextoptions <- renderUI({
    dropdown(
      shinydashboard::box(
        title = p(trloc("options.text"), actionButton("resetuiTextoptions", trloc("options.graphs.reset"))),
        status = "primary", solidHeader = TRUE, width = 12, background = "black", collapsible = TRUE, collapsed = FALSE,
        fluidRow(
          column(
            12,
            popify(
              textInput("textMain", label = h6(trloc("options.text.main.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = trloc("options.text.main.label")),
              title = trloc("options.text.main.label"), content = trloc("options.text.main.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          )
        ),
        fluidRow(
          column(
            6,
            popify(
              textInput("textY", label = h6(trloc("options.text.yaxis.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = trloc("options.text.yaxis.label")),
              title = trloc("options.text.yaxis.label"), content = trloc("options.text.yaxis.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          ),
          column(
            6,
            popify(
              textInput("textX", label = h6(trloc("options.text.xaxis.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = trloc("options.text.xaxis.label")),
              title = trloc("options.text.xaxis.label"), content = trloc("options.text.xaxis.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          )
        )
      ),
      circle = TRUE,
      tooltip = tooltipOptions(placement = "left", title = trloc("options.text"), html = TRUE),
      margin = "0px",
      style = "minimal",
      icon = icon("fas fa-text-height"),
      status = "primary",
      width = "400px",
      right = TRUE,
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInRight,
        exit = animations$fading_exits$fadeOutRight
      )
    )
  })

  output$uiGraphoptions <- renderUI({
    colObservedLines.list <- as.list(c("default", colors()))
    names(colObservedLines.list) <- c(trloc("app.default"), colors())
    colThresholds.list <- as.list(c("default", rownames(brewer.pal.info), colors()))
    names(colThresholds.list) <- c(trloc("app.default"), rownames(brewer.pal.info), colors())
    dropdown(
      shinydashboard::box(
        title = p(trloc("options.graphs"), actionButton("resetuiGraphoptions", trloc("options.graphs.reset"))),
        status = "primary", solidHeader = TRUE, width = 12, background = "black", collapsible = TRUE, collapsed = FALSE,
        fluidRow(
          column(
            6,
            popify(
              selectInput("colObservedLines", h6(trloc("options.graphs.observedline.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = colObservedLines.list, size = 1, selectize = FALSE, selected = default_values$colObservedLines),
              title = trloc("options.graphs.observedline.label"), content = trloc("options.graphs.observedline.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          ),
          column(
            6,
            popify(
              selectInput("colObservedPoints", h6(trloc("options.graphs.observedpoints.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = colObservedLines.list, size = 1, selectize = FALSE, selected = default_values$colObservedPoints),
              title = trloc("options.graphs.observedpoints.label"), content = trloc("options.graphs.observedpoints.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          )
        ),
        fluidRow(
          column(
            6,
            popify(
              selectInput("colEpidemicStart", h6(trloc("options.graphs.epidemicstart.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = colObservedLines.list, size = 1, selectize = FALSE, selected = default_values$colEpidemicStart),
              title = trloc("options.graphs.epidemicstart.label"), content = trloc("options.graphs.epidemicstart.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          ),
          column(
            6,
            popify(
              selectInput("colEpidemicStop", h6(trloc("options.graphs.epidemicend.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = colObservedLines.list, size = 1, selectize = FALSE, selected = default_values$colEpidemicStop),
              title = trloc("options.graphs.epidemicend.label"), content = trloc("options.graphs.epidemicend.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          )
        ),
        fluidRow(
          column(
            6,
            popify(
              selectInput("colThresholds", h6(trloc("options.graphs.thresholdspalette.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = colThresholds.list, size = 1, selectize = FALSE, selected = default_values$colThresholds),
              title = trloc("options.graphs.thresholdspalette.label"), content = trloc("options.graphs.thresholdspalette.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          ),
          column(
            6,
            popify(
              selectInput("colLevels", h6(trloc("options.graphs.levelspalette.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = colThresholds.list, size = 1, selectize = FALSE, selected = default_values$colLevels),
              title = trloc("options.graphs.levelspalette.label"), content = trloc("options.graphs.levelspalette.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          )
        ),
        fluidRow(
          column(
            6,
            popify(
              selectInput("colSeasons", h6(trloc("options.graphs.seasonspalette.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = colThresholds.list, size = 1, selectize = FALSE, selected = default_values$colSeasons),
              title = trloc("options.graphs.seasonspalette.label"), content = trloc("options.graphs.seasonspalette.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          ),
          column(
            6,
            popify(
              selectInput("colEpidemic", h6(trloc("options.graphs.timingpalette.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = colThresholds.list, size = 1, selectize = FALSE, selected = default_values$colEpidemic),
              title = trloc("options.graphs.timingpalette.label"), content = trloc("options.graphs.timingpalette.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          )
        ),
        fluidRow(
          column(
            12,
            popify(
              shinyWidgets::prettyCheckbox(
                inputId = "yaxis0",
                label = trloc("options.graphs.yaxis0.label"),
                value = default_values$yaxis0,
                shape = "curve"
              ),
              title = trloc("options.graphs.yaxis0.label"), content = trloc("options.graphs.yaxis0.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          )
        )
      ),
      circle = TRUE,
      tooltip = tooltipOptions(placement = "left", title = trloc("options.graphs"), html = TRUE),
      margin = "0px",
      style = "minimal",
      icon = icon("fas fa-palette"),
      status = "primary",
      width = "400px",
      right = TRUE,
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInRight,
        exit = animations$fading_exits$fadeOutRight
      )
    )
  })

  output$uiMEMoptions <- renderUI({
    method.list <- list("Original method" = 1, "Fixed criterium method" = 2, "Slope method" = 3, "Second derivative method" = 4)
    names(method.list) <- c(trloc("options.mem.timing.method.original"), trloc("options.mem.timing.method.fixed"), trloc("options.mem.timing.method.slope"), trloc("options.mem.timing.method.derivative"))
    nvalues.list <- list("30 in total" = -1, "All" = 0, "1/season" = 1, "2/season" = 2, "3/season" = 3, "4/season" = 4, "5/season" = 5, "6/season" = 6, "7/season" = 7, "8/season" = 8, "9/season" = 9, "10/season" = 10)
    names(nvalues.list) <- c(trloc("options.mem.thresholds.values.30"), trloc("options.mem.thresholds.values.all"), trloc("options.mem.thresholds.values.1"), trloc("options.mem.thresholds.values.2"), trloc("options.mem.thresholds.values.3"), trloc("options.mem.thresholds.values.4"), trloc("options.mem.thresholds.values.5"), trloc("options.mem.thresholds.values.6"), trloc("options.mem.thresholds.values.7"), trloc("options.mem.thresholds.values.8"), trloc("options.mem.thresholds.values.9"), trloc("options.mem.thresholds.values.10"))
    validation.list <- list("Cross" = "cross", "Sequential" = "sequential")
    names(validation.list) <- c(trloc("options.mem.goodness.validation.cross"), trloc("options.mem.goodness.validation.sequential"))
    optimmethod.list <- list("Positive likehood" = "pos.likehood", "Negative likehood" = "neg.likehood", "Aditive" = "aditive", "Multiplicative" = "multiplicative", "Mixed" = "mixed", "Percent agreement" = "percent", "Matthews Correlation Coefficient" = "matthews", "Youden's Index" = "youden")
    names(optimmethod.list) <- c(trloc("options.mem.goodness.optimization.positive"), trloc("options.mem.goodness.optimization.negative"), trloc("options.mem.goodness.optimization.aditive"), trloc("options.mem.goodness.optimization.multiplicative"), trloc("options.mem.goodness.optimization.mixed"), trloc("options.mem.goodness.optimization.percent"), trloc("options.mem.goodness.optimization.matthews"), trloc("options.mem.goodness.optimization.youden"))
    type.list <- list("Arithmetic mean and mean confidence interval" = 1, "Geometric mean and mean confidence interval" = 2, "Median and Nyblom confidence interval" = 3, "Median and bootstrap confidence interval" = 4, "Arithmetic mean and point confidence interval" = 5, "Geometric mean and point confidence interval" = 6)
    names(type.list) <- c(trloc("options.mem.thresholds.epidemic.arithmeticmean"), trloc("options.mem.thresholds.epidemic.geometricmean"), trloc("options.mem.thresholds.epidemic.mediannyblom"), trloc("options.mem.thresholds.epidemic.medianbootstrap"), trloc("options.mem.thresholds.epidemic.arithmeticprediction"), trloc("options.mem.thresholds.epidemic.geometricprediction"))
    centering.list <- list(
      "Highest mean duration-weeks period" = -1, "Highest 1-week period (peak)" = 1, "Highest 2-weeks period" = 2, "Highest 3-weeks period" = 3, "Highest 4-weeks period" = 4, "Highest 5-weeks period" = 5, "Highest 6-weeks period" = 6, "Highest 7-weeks period" = 7,
      "Highest 8-weeks period" = 8, "Highest 9-weeks period" = 9, "Highest 10-weeks period" = 10, "Highest 11-weekss period" = 11, "Highest 12-weeks period" = 12, "Highest 13-weeks period" = 13, "Highest 14-weeks period" = 14, "Highest 15-weeks period" = 15
    )
    names(centering.list) <- c(trloc("options.mem.other.centering.mean"), trloc("options.mem.other.centering.1"), trloc("options.mem.other.centering.2"), trloc("options.mem.other.centering.3"), trloc("options.mem.other.centering.4"), trloc("options.mem.other.centering.5"), trloc("options.mem.other.centering.6"), trloc("options.mem.other.centering.7"), trloc("options.mem.other.centering.8"), trloc("options.mem.other.centering.9"), trloc("options.mem.other.centering.10"), trloc("options.mem.other.centering.11"), trloc("options.mem.other.centering.12"), trloc("options.mem.other.centering.13"), trloc("options.mem.other.centering.14"), trloc("options.mem.other.centering.15"))
    dropdown(
      shinydashboard::box(
        title = p(trloc("options.mem"), actionButton("resetuiMEMoptions", label = trloc("options.graphs.reset"))),
        status = "danger", solidHeader = FALSE, width = 12, background = "navy", collapsible = TRUE, collapsed = FALSE,
        h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("options.mem.timing")),
        fluidRow(
          column(6, offset = 0, popify(
            selectInput("method", h6(trloc("options.mem.timing.method.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = method.list, size = 1, selectize = FALSE, selected = default_values$method),
            title = trloc("options.mem.timing.method.label"), content = trloc("options.mem.timing.method.hint"), placement = "left", trigger = "focus", options = list(container = "body")
          )),
          column(6, offset = 0, conditionalPanel(
            condition = "input.method == 2",
            popify(
              numericInput("param", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("options.mem.timing.slope.label")), value = default_values$param$value, min = default_values$param$min, max = default_values$param$max, step = default_values$param$step),
              title = trloc("options.mem.timing.slope.label"), content = trloc("options.mem.timing.slope.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          ))
        ),
        hidden(fluidRow(
          column(12, offset = 0, popify(
            numericInput("optimparam", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("main.model.optimize.automatic.optimumparam")), value = default_values$optimparam$value, min = default_values$optimparam$min, max = default_values$optimparam$max, step = default_values$optimparam$step),
            title = trloc("main.model.optimize.automatic.optimumparam"), content = trloc("main.model.optimize.automatic.optimumparam"), placement = "left", trigger = "focus", options = list(container = "body")
          ))
        )),
        h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("options.mem.thresholds")),
        fluidRow(
          column(
            6,
            popify(
              selectInput("nvalues", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("options.mem.thresholds.values.label")), choices = nvalues.list, size = 1, selectize = FALSE, selected = default_values$nvalues),
              title = trloc("options.mem.thresholds.values.label"), content = trloc("options.mem.thresholds.values.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          ),
          column(
            6,
            popify(
              numericInput("ntails", h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("options.mem.thresholds.tails.label.label")), value = default_values$ntails$value, min = default_values$ntails$min, max = default_values$ntails$max, step = default_values$ntails$step),
              title = trloc("options.mem.thresholds.tails.label.label"), content = trloc("options.mem.thresholds.tails.label.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          )
        ),
        fluidRow(
          column(6, offset = 0, popify(
            selectInput("typethreshold", h6(trloc("options.mem.thresholds.epidemic.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = type.list, size = 1, selectize = FALSE, selected = default_values$typethreshold),
            title = trloc("options.mem.thresholds.epidemic.label"), content = trloc("options.mem.thresholds.epidemic.hint"), placement = "left", trigger = "focus", options = list(container = "body")
          )),
          column(6, offset = 0, popify(
            selectInput("typeintensity", h6(trloc("options.mem.thresholds.intensity.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = type.list, size = 1, selectize = FALSE, selected = default_values$typeintensity),
            title = trloc("options.mem.thresholds.intensity.label"), content = trloc("options.mem.thresholds.intensity.hint"), placement = "left", trigger = "focus", options = list(container = "body")
          ))
        ),
        conditionalPanel(
          condition = "input.advanced",
          fluidRow(
            column(3, offset = 0, ),
            column(9, offset = 0, popify(
              shinyWidgets::materialSwitch(
                inputId = "usetdistribution",
                label = trloc("options.mem.thresholds.epidemic.uset.label"),
                value = default_values$usetdistribution,
                right = TRUE,
                status = "info"
              ),
              title = trloc("options.mem.thresholds.epidemic.uset.label"),
              content = trloc("options.mem.thresholds.epidemic.uset.hint"),
              placement = "right", trigger = "focus", options = list(container = "body")
            ))
          )
        ),
        fluidRow(
          column(
            4,
            popify(
              numericInput("levelintensitym", h6(trloc("options.mem.thresholds.intensity.medium.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = default_values$levelintensitym$value, min = default_values$levelintensitym$min, max = default_values$levelintensitym$max, step = default_values$levelintensitym$step),
              title = trloc("options.mem.thresholds.intensity.medium.label"), content = trloc("options.mem.thresholds.intensity.medium.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          ),
          column(
            4,
            popify(
              numericInput("levelintensityh", h6(trloc("options.mem.thresholds.intensity.high.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = default_values$levelintensityh$value, min = default_values$levelintensityh$min, max = default_values$levelintensityh$max, step = default_values$levelintensityh$step),
              title = trloc("options.mem.thresholds.intensity.high.label"), content = trloc("options.mem.thresholds.intensity.high.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          ),
          column(
            4,
            popify(
              numericInput("levelintensityv", h6(trloc("options.mem.thresholds.intensity.veryhigh.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = default_values$levelintensityv$value, min = default_values$levelintensityv$min, max = default_values$levelintensityv$max, step = default_values$levelintensityv$step),
              title = trloc("options.mem.thresholds.intensity.veryhigh.label"), content = trloc("options.mem.thresholds.intensity.veryhigh.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          )
        ),
        h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("options.mem.goodness")),
        fluidRow(
          column(
            6,
            popify(
              selectInput("validation", h6(trloc("options.mem.goodness.validation.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = validation.list, size = 1, selectize = FALSE, selected = default_values$validation),
              title = trloc("options.mem.goodness.validation.label"), content = trloc("options.mem.goodness.validation.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          ),
          column(
            6,
            popify(
              selectInput("optimmethod", h6(trloc("options.mem.goodness.optimization.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = optimmethod.list, size = 1, selectize = FALSE, selected = default_values$optimmethod),
              title = trloc("options.mem.goodness.optimization.label"), content = trloc("options.mem.goodness.optimization.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          )
        ),
        popify(
          sliderInput("paramrange", label = h6(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("options.mem.goodness.optimization.paramrange.label")), value = default_values$paramrange$value, min = default_values$paramrange$min, max = default_values$paramrange$max, step = default_values$paramrange$step),
          title = trloc("options.mem.goodness.optimization.paramrange.label"), content = trloc("options.mem.goodness.optimization.paramrange.hint"), placement = "left", trigger = "focus", options = list(container = "body")
        ),
        conditionalPanel(
          condition = "input.showadvanced",
          fluidRow(
            column(
              6,
              popify(
                numericInput("minsensitivity", h6(trloc("options.mem.goodness.optimization.minsensitivity.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = default_values$minsensitivity$value, min = default_values$minsensitivity$min, max = default_values$minsensitivity$max, step = default_values$minsensitivity$step),
                title = trloc("options.mem.goodness.optimization.minsensitivity.label"), content = trloc("options.mem.goodness.optimization.minsensitivity.hint"), placement = "left", trigger = "focus", options = list(container = "body")
              )
            ),
            column(
              6,
              popify(
                numericInput("minspecificity", h6(trloc("options.mem.goodness.optimization.minspecificity.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = default_values$minspecificity$value, min = default_values$minspecificity$min, max = default_values$minspecificity$max, step = default_values$minspecificity$step),
                title = trloc("options.mem.goodness.optimization.minspecificity.label"), content = trloc("options.mem.goodness.optimization.minspecificity.hint"), placement = "left", trigger = "focus", options = list(container = "body")
              )
            )
          )
        ),
        h4(tags$style(type = "text/css", "#q1 {vertical-align: top;}"), trloc("options.mem.other")),
        fluidRow(
          column(6, offset = 0, popify(
            selectInput("typeaveragecurve", h6(trloc("options.mem.other.typeaveragecurve.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = type.list, size = 1, selectize = FALSE, selected = default_values$typeaveragecurve),
            title = trloc("options.mem.other.typeaveragecurve.label"), content = trloc("options.mem.other.typeaveragecurve.hint"), placement = "left", trigger = "focus", options = list(container = "body")
          )),
          column(6, offset = 0, popify(
            selectInput("typeother", h6(trloc("options.mem.other.typeother.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = type.list, size = 1, selectize = FALSE, selected = default_values$typeother),
            title = trloc("options.mem.other.typeother.label"), content = trloc("options.mem.other.typeother.hint"), placement = "left", trigger = "focus", options = list(container = "body")
          ))
        ),
        fluidRow(
          column(6, offset = 0, popify(
            numericInput("levelaveragecurve", h6(trloc("options.mem.other.levelaveragecurve.level"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = default_values$levelaveragecurve$value, min = default_values$levelaveragecurve$min, max = default_values$levelaveragecurve$max, step = default_values$levelaveragecurve$step),
            title = trloc("options.mem.other.levelaveragecurve.level"), content = trloc("options.mem.other.levelaveragecurve.hint"), placement = "left", trigger = "focus", options = list(container = "body")
          )),
          column(6, offset = 0, popify(
            numericInput("levelother", h6(trloc("options.mem.other.levelother.level"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), value = default_values$levelother$value, min = default_values$levelother$min, max = default_values$levelother$max, step = default_values$levelother$step),
            title = trloc("options.mem.other.levelother.level"), content = trloc("options.mem.other.levelother.hint"), placement = "left", trigger = "focus", options = list(container = "body")
          ))
        ),
        fluidRow(
          column(12, offset = 0, conditionalPanel(
            condition = "input.advanced",
            popify(
              selectInput("centering", h6(trloc("options.mem.other.centering.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")), choices = centering.list, size = 1, selectize = FALSE, selected = default_values$centering),
              title = trloc("options.mem.other.centering.label"), content = trloc("options.mem.other.centering.hint"), placement = "left", trigger = "focus", options = list(container = "body")
            )
          ))
        )
      ),
      circle = TRUE,
      tooltip = tooltipOptions(placement = "left", title = trloc("options.mem"), html = TRUE),
      margin = "0px",
      style = "minimal",
      icon = icon("fas fa-cogs"),
      status = "danger",
      width = "400px",
      right = TRUE,
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInRight,
        exit = animations$fading_exits$fadeOutRight
      )
    )
  })

  output$uiSupport <- renderUI({
    dropdown(
      shinydashboard::box(
        shinyjs::useShinyjs(),
        title = trloc("options.support"), status = "info", solidHeader = TRUE, width = 12, background = "black", collapsible = TRUE, collapsed = FALSE,
        conditionalPanel(condition = "input.experimental", h5(a(trloc("options.support.manual"), href = "https://github.com/lozalojo/memapp/blob/assets/technicalmanualdev.pdf?raw=true", target = "_blank"))),
        conditionalPanel(condition = "!input.experimental", h5(a(trloc("options.support.manual"), href = "https://github.com/lozalojo/memapp/blob/assets/technicalmanual.pdf?raw=true", target = "_blank"))),
        h5(a(trloc("options.support.issues"), href = "https://github.com/lozalojo/memapp/issues", target = "_blank")),
        hidden(popify(
          shinyWidgets::prettyCheckbox(
            inputId = "showadvanced",
            label = trloc("app.showadvancedtickbox"),
            value = default_values$showadvanced,
            shape = "curve"
          ),
          title = trloc("app.showadvancedtickbox"), content = trloc("app.showadvancedtickbox"), placement = "left", trigger = "focus", options = list(container = "body")
        )),
        conditionalPanel(
          condition = "input.showadvanced",
          popify(
            shinyWidgets::prettyCheckbox(
              inputId = "advanced",
              label = trloc("options.support.advancedfeatures.label"),
              value = default_values$advanced,
              shape = "curve"
            ),
            title = trloc("options.support.advancedfeatures.hint"), content = trloc("options.support.advancedfeatures.hint"), placement = "left", trigger = "focus", options = list(container = "body")
          )
        ),
        hidden(popify(
          shinyWidgets::prettyCheckbox(
            inputId = "showexperimental",
            label = trloc("app.showexperimentaltickbox"),
            value = default_values$showexperimental,
            shape = "curve"
          ),
          title = trloc("app.showexperimentaltickbox"), content = trloc("app.showexperimentaltickbox"), placement = "left", trigger = "focus", options = list(container = "body")
        )),
        conditionalPanel(
          condition = "input.showexperimental",
          popify(
            shinyWidgets::prettyCheckbox(
              inputId = "experimental",
              label = trloc("options.support.experimentalfeatures.label"),
              value = default_values$experimental,
              shape = "curve"
            ),
            title = trloc("options.support.experimentalfeatures.hint"), content = trloc("options.support.experimentalfeatures.hint"), placement = "left", trigger = "focus", options = list(container = "body")
          )
        )
      ),
      circle = TRUE,
      tooltip = tooltipOptions(placement = "left", title = trloc("options.support"), html = TRUE),
      margin = "0px",
      style = "minimal",
      icon = icon("fas fa-info"),
      status = "royal",
      width = "400px",
      right = TRUE,
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInRight,
        exit = animations$fading_exits$fadeOutRight
      )
    )
  })

  output$uiLanguage <- renderUI({
    popify(
      h4(trloc("options.language.label"), tags$style(type = "text/css", "#q1 {vertical-align: top;}")),
      title = trloc("options.language.label"), content = trloc("options.language.hint"), placement = "left", trigger = "focus", options = list(container = "body")
    )
  })

  output$uiLanguageDefault <- renderUI({
    popify(
      title = trloc("options.language.default.label"), content = trloc("options.language.default.hint"), placement = "rigth", actionButton("makeLangDefault", trloc("options.language.default.label"))
    )
  })

  #####################################
  ### ENDING
  #####################################

  session$onSessionEnded(function() {
    stopApp()
  })
})
