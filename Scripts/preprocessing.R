# Before converting the original dataset into msdata format check (to follow our procedure):
# - No extraordinary stops, no holidays and only week days;
# - Delays: Arrival Delays;
# - Segmentation: a) Direction (0,1); b) Time slot (Morning-, Off-, Evening- peak); c) Route section (Zones: 1,2,3,4);
# - Status: Delay escalation;
# - Time: Time spent in each status.

# Loading data and preparation
data=read.csv("Data/data.csv")
data$date=as.Date(data$date, format = "%d/%m/%Y")
data$departure=as.POSIXct(data$departure, format = "%d/%m/%Y %H:%M:%S")
data$arrival=as.POSIXct(data$arrival, format = "%d/%m/%Y %H:%M:%S")
data$ora_entry_time=as.POSIXct(data$entry_time, format = "%d/%m/%Y %H:%M:%S")
data$exit_time=as.POSIXct(data$exit_time, format = "%d/%m/%Y %H:%M:%S")
str(data)


# MSDATA: At this point we construct an object of type \code{msdata} to allow us 
# to use \code{mstate in combination with \code{survival} and \code{flexsurv} 
# (only for parametric models). Since our transition matrix is circular and reversible, 
# we will use a function different from \code{msprep}, but able to reach the same 
# type of results.

data = data[order(data$departure, data$entry_time), ]
row.names(data) <- NULL

#function msprep2 (available at https://rdrr.io/cran/multistateutils/src/R/msprep2.R):
get_sink_states <- function(tmat) {
  # Find rows with missing values
  sink_states <- which(apply(tmat, 1, function(row) all(is.na(row))))
  return(sink_states)
}
msprep2 <- function(entry, tmat, censors=NULL,
                    start_times=NULL, start_states=NULL, covars=NULL,
                    idcol='id') {
  
  DEFAULT_START_TIME <- 0
  DEFAULT_START_STATE <- 1
  
  # R CMD CHECK
  id <- NULL
  Tstop <- NULL
  Tstart <- NULL
  state <- NULL
  prev_state <- NULL
  start_state <- NULL
  start_time <- NULL
  time <- NULL
  status <- NULL
  from <- NULL
  to <- NULL
  to.actual <- NULL
  to.possible <- NULL
  trans <- NULL
  censor_time <- NULL
  
  if (!idcol %in% colnames(entry))
    stop(paste0("Error: id field '", idcol, "' not found in entry."))
  if (!'time' %in% colnames(entry))
    stop("Error: column 'time' not found in entry.")
  if (!'state' %in% colnames(entry))
    stop("Error: column 'state' not found in entry.")
  
  entry <- entry %>%
    dplyr::rename(id = idcol, Tstop=time)
  # Build up list of unique_ids
  unique_ids <- unique(entry$id)
  nstates <- ncol(tmat)
  state_names <- colnames(tmat)
  
  if (!is.null(censors)) {
    censors <- censors %>%
      dplyr::rename(id = idcol)
    unique_ids <- union(unique_ids, unique(censors$id))
  }
  if (!is.null(start_times)) {
    if (!idcol %in% colnames(start_times))
      stop(paste0("Error: id field '", idcol, "' not found in start_times."))
    if (!'start_time' %in% colnames(start_times))
      stop(paste0("Error: column start_time not found in start_times."))
    start_times <- start_times %>%
      dplyr::rename(id = idcol)
    unique_ids <- union(unique_ids, unique(start_times$id))
  }
  if (!is.null(start_states)) {
    if (!idcol %in% colnames(start_states))
      stop(paste0("Error: id field '", idcol, "' not found in start_states."))
    if (!'start_state' %in% colnames(start_states))
      stop(paste0("Error: column start_state not found in start_states."))
    
    ss <- start_states$start_state
    if (is.factor(ss)) {
      ss <- as.character(ss)
      start_states$start_state <- as.character(start_states$start_state)
    }
    
    if (!(is.numeric(ss) || is.character(ss)))
      stop("Error: start_state column must be state name or number.")
    if (is.numeric(ss)) {
      if (!all((ss %% 1) == 0))
        stop("Error: start_state column must be state name or number.")
      if (max(ss) > nstates || min(ss) < 1)
        stop("Error: start_state column must be state name or number.")
    }
    if (is.character(ss)) {
      if (!all(ss %in% state_names))
        stop("Error: start_state column must be state name or number.")
    }
    
    start_states <- start_states %>%
      dplyr::rename(id = idcol)
    unique_ids <- union(unique_ids, unique(start_states$id))
  }
  if (!is.null(covars)) {
    if (!idcol %in% colnames(covars))
      stop(paste0("Error: id field '", idcol, "' not found in covars."))
    covars <- covars %>%
      dplyr::rename(id = idcol)
    unique_ids <- union(unique_ids, unique(covars$id))
  }
  
  if (is.null(start_states))
    start_states <- data.frame(id=unique_ids, start_state=DEFAULT_START_STATE)
  if (is.null(start_times))
    start_times <- data.frame(id=unique_ids, start_time=DEFAULT_START_TIME)
  
  # Guards
  # Check that every individual has unique time
  has_dups <- entry %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(has_duplicate_times=sum(duplicated(Tstop))>0)
  if (sum(has_dups$has_duplicate_times) > 0) {
    stop("Error: each id in entry must have unique state entry times.")
  }
  
  # Convert state names to numbers
  if (is.character(entry$state) || is.factor(entry$state))
    entry$state <- match(entry$state, state_names)
  if (is.character(start_states$start_state) || is.factor(start_states$start_state))
    start_states$start_state <- match(start_states$start_state, state_names)
  
  ntrans <- sum(!is.na(tmat))
  ninds <- length(unique_ids)
  
  # Now need to add starting state and times.
  # Firstly, obtain the rank order of each state entry
  entry2 <- entry %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(entry_order = dplyr::row_number(Tstop)) %>%
    dplyr::mutate(prev_state = dplyr::lag(state)) %>%
    dplyr::left_join(start_states, by='id') %>%
    dplyr::mutate(start_state = ifelse(is.na(start_state), DEFAULT_START_STATE, start_state),
                  prev_state = ifelse(is.na(prev_state), start_state, prev_state)) %>%
    dplyr::select(-start_state)
  
  # Likewise, obtain previous times
  entry3 <- entry2 %>%
    dplyr::mutate(Tstart = dplyr::lag(Tstop)) %>%
    dplyr::left_join(start_times, by='id') %>%
    dplyr::mutate(start_time = ifelse(is.na(start_time), DEFAULT_START_TIME, start_time),
                  Tstart = ifelse(is.na(Tstart), start_time, Tstart)) %>%
    dplyr::select(-start_time) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(time = Tstop - Tstart, status=1) %>%
    dplyr::select(id, from=prev_state, to=state, Tstart, Tstop, time, status)
  
  # Now need to add the transitions that werent made
  # Need DF of all possible ids and transitions
  sink_states <- match(get_sink_states(tmat), state_names)
  trans_ids <- tmat[!is.na(tmat)]
  
  tmat_long <- dplyr::bind_rows(lapply(stats::setNames(trans_ids, trans_ids), function(transition) {
    index <- which(tmat == transition) - 1  # Easier to do column/row index arithmetic in zero base
    from <- (index %% nstates) + 1          # Convert back to 1-index
    to <- floor(index / nstates) + 1        # Ditto
    data.frame(from, to)
  }), .id='trans')
  
  tmat_withids <- cbind(tmat_long[rep(1:nrow(tmat_long), ninds), ],
                        id=rep(unique_ids, each=ntrans))
  
  # Now join this onto main entry to get possible 'to' states
  entry4 <- entry3 %>%
    dplyr::left_join(tmat_withids, by=c('id', 'from'), suffix=c('.actual', '.possible')) %>%
    dplyr::mutate(status = ifelse(to.actual == to.possible, status, 0)) %>%
    dplyr::select(id, from, to=to.possible, trans, Tstart, Tstop, time, status)   # Clean up
  
  if (!is.null(censors)) {
    # Two types of people who have useful censor information.
    #   1. Those who never entered any state
    #   2. Those who haven't entered a sink state yet.
    # Can only handle these cases if have their time of last follow up!
    
    # Identify those who never entered any state
    never_entered_state <- start_states %>%
      dplyr::left_join(entry, by='id') %>%
      dplyr::filter(is.na(Tstop)) %>%
      dplyr::select(id, start_state) %>%
      dplyr::left_join(start_times, by='id') %>%
      dplyr::mutate(start_time = ifelse(is.na(start_time), DEFAULT_START_TIME, start_time)) %>%
      dplyr::rename(from=start_state, Tstart=start_time)
    
    # And identify those who joined the system but never entered sink state
    last_states <- entry4 %>%
      dplyr::filter(status == 1) %>%
      dplyr::group_by(id) %>%
      dplyr::top_n(1, Tstop) %>%
      dplyr::filter(!to %in% sink_states) %>%
      dplyr::select(id, to, Tstop) %>%
      dplyr::ungroup() %>%
      dplyr::rename(from=to, Tstart=Tstop)
    
    # Then combine them and work out their censored transitions
    censored_trans <- never_entered_state %>%
      rbind(last_states) %>%
      dplyr::left_join(tmat_long, by='from') %>%
      dplyr::left_join(censors, by='id') %>%
      dplyr::mutate(status=0, time=censor_time - Tstart) %>%
      dplyr::select(id, from, to, trans, Tstart, Tstop=censor_time, time, status)
    
    # And finally add these censored observations to existing long DF
    entry4 <- entry4 %>%
      rbind(censored_trans)
  }
  
  if (!is.null(covars)) {
    entry4 <- entry4 %>%
      dplyr::left_join(covars, by='id')
  }
  
  to_int <- c('id', 'from', 'to', 'trans', 'status')
  entry4[to_int] <- lapply(entry4[to_int], as.integer)
  
  out <- entry4 %>%
    dplyr::arrange(id, Tstart, trans)
  attr(out, "trans") <- tmat
  class(out) <- c('msdata', class(out))
  out
}


# Now, we define our transition matrix and we then convert our dataset using \code{msprep2}.
#Transition matrix:
# 3-state model:
tmat <- transMat(x = list( c(2), c(1, 3), c(2) ), names = c("OnTime", "MildDelay", "SevereDelay"))
# 4-state model:
# tmat <- transMat(x = list( c(2), c(1, 3), c(2, 4), c(3) ), names = c("OnTime", "MildDelay", "MediumDelay", "SevereDelay"))
tmat
is.circular(tmat)

#Preprocessing to use msprep2:
#Insert an id:
data <- data %>%
  mutate(id = dense_rank(ID_Train))
#entry: id, state, time
entry <- data %>% dplyr::select(all_of(c("id", "status", "time")))
names(entry)[2]="state"
entry <- entry %>%
  arrange(id, time)
entry <- entry %>%
  distinct(id, time, .keep_all = TRUE)
#start_times: id, time
start_times <- data %>%
  group_by(id) %>%
  slice_head(n = 1)
start_times=start_times[, c("id", "time")]
names(start_times)[2]="start_time"
#start_states: id, state
start_states <- data %>%
  group_by(id) %>%
  slice_head(n = 1)
start_states_S5=start_states[, c("id", "status")]
names(start_states)[2]="start_state"
#censors:
censors <- entry %>%
  dplyr::group_by(id) %>%
  dplyr::filter(!sink_state %in% state) %>%
  dplyr::summarise(censor_time = max(time, na.rm = TRUE)) %>%
  dplyr::ungroup()
#covariates:
covars <- data %>%
  group_by(id) %>%
  slice_head(n = 1)
covars=covars[, c("id", "ID_Train", "ID_TrainRun", "name", "date", "name_d", "name_a", "departure", "arrival", "boarded", "alighted", "on_board", "direction", "time_slot", "zone")]

#We can now apply \code{msprep2} function:
msdata <- msprep2(entry, tmat, censors=censors, start_times=start_times, start_states=start_states, covars=covars, idcol = "id")
msdata <- msdata[msdata$Tstop > msdata$Tstart, ]
rownames(msdata) <- NULL
any(is.na(msdata))
msdata<-na.omit(msdata)
msdata$trans=as.factor(msdata$trans)
attr(msdata, "trans") <- tmat

# # If needed:
# acc <- list()
# prev_from <- msdata$from[1]
# prev_trip <- msdata$trip_id[1]
# baseline <- 0
# for (i in 1:nrow(msdata)) {
#   
#   f <- msdata$from[i]
#   t <- msdata$to[i]
#   tm <- msdata$time[i]
#   trip <- msdata$trip_id[i] 
#   if (i == 1 || f != prev_from || trip != prev_trip) {
#     acc <- list()
#     baseline <- 0
#   }
#   if (!(t %in% names(acc))) {
#     acc[[as.character(t)]] <- baseline
#   }
#   acc[[as.character(t)]] <- acc[[as.character(t)]] + tm
#   msdata$time[i] <- acc[[as.character(t)]]
#   prev_from <- f
#   prev_trip <- trip
# }
# attr(msdata, "trans") <- tmat


# Preliminary analysis: total number of events and proportions.
events(msdata)


# Since the previous function cannot deal with covariates that can change in time, 
# use a simple for-cycle to assign the unit-specific values. NB: Faster procedures 
# are not adopted to prevent the disappearence of the dataset-attribute \code{msdata}, 
# crucial for this type of analysis.
msdata_cov <- msdata
msdata_cov$zone <- NA
msdata_cov$name <- NA
msdata_cov$boarded <- NA
msdata_cov$alighted <- NA

data$zone <- as.character(data$zone)
for (i in 1:nrow(msdata_cov)) {
  id_train <- msdata_cov$ID_Train[i]
  time_stop <- msdata_cov$Tstop[i]
  for (j in 1:nrow(data)) {
    if (data$ID_Train[j] == id_train && data$time[j] == time_stop) {
      msdata_cov$zone[i] <- data$zone[j]
      msdata_cov$name[i] <- data$name[j]
      msdata_cov$boarded[i] <- data$boarded[j]
      msdata_cov$alighted[i] <- data$alighted[j]
      break
    }
  }
}

data$zone <- as.factor(data$zone)
msdata_cov$zone <- as.factor(msdata_cov$zone)
attr(msdata_cov, "trans") <- tmat