###Audio Processing###

###1) Reading Sound

install.packages("tuneR")
install.packages("seewave")
install.packages("wrassp")
library(readr)


#First, analyze the sound wave by reading the the audio file using "tuneR" package :
library(tuneR, warn.conflicts = F, quietly = T)
fin = 'C:/Users/user/Desktop/hrtf_project/datafiles/river-6.wav'

riversound = readWave(fin)
summary(riversound)

#Another way of analyses of the sound wave using "wrassp" package :
library(wrassp)
# create path to wav file
file_audio_path <- "C:/Users/user/Desktop/hrtf_project/datafiles/river-6.wav"
# read audio file
au = read.AsspDataObj(file_audio_path)
str(au)

#compute the formant and bandwidth with wrassp
# calculate formants and corresponding bandwidth values
#fmBwVals = forest(file_audio_path, toFile=F)
#fmBwVals

#Waveform-------------------------------------------------------------------------------

#extract signal
snd = riversound@left

# determine duration
dur = length(snd)/riversound@samp.rate
dur # seconds

# determine sample rate
fs = riversound@samp.rate
fs # Hz


#Plot Waveform
# demean to remove DC offset
snd = snd - mean(snd)

# plot waveform
plot(snd, type = 'l', xlab = 'Samples', ylab = 'Amplitude')


#TONE---------------------------------------------------------------------------

#A time representation of the sound can be obtained by plotting the pressure values against the time axis. However we need to create an array containing the time points first:
  
timeArray <- (0:(5292-1)) / sndObj@samp.rate
timeArray <- timeArray * 1000 #scale to milliseconds
plot(timeArray, s1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude') 

####################
# Load required libraries
library(tuneR)

# File path to your sound file
file_path <- "C:/Users/user/Desktop/hrtf_project/datafiles/river-6.wav"

# Load the audio file
audio <- readWave(file_path)

# Extract the mono waveform from the left channel
s1 <- audio@left

# Create an array containing the time points in milliseconds
timeArray <- (0:(length(s1)-1)) / audio@samp.rate * 1000  # scale to milliseconds

# Plot the time representation
plot(timeArray, s1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude')



#---------------2D Spectogram---------------------------------------------------------------------------------------

#with oscillogram plot
data(tico)
spectro(tico,f=22050,
        ovlp=50,zp=16,
        collevels=seq(-40,0,0.5),
        osc=TRUE)

#colour modifications
data(peewit)
spectro(peewit,f=22050,
        palette=temp.colors,
        collevels=seq(-100,0,1))



#---------------3D Spectogram----------------------------------------------------------------------------------------

#false 3D (waterfall)
data(tico)
wf(tico,f=22050,
   ovlp=50,hoff=0,voff=2,
   border="#00000075")


########################

data(tico)
wf(tico,f=22050,
   ovlp=50,hoff=0,voff=2,
   border="#00000075")

#######################


#Spectogram----river-sound-----------------------------------------------------------------
install.packages("av")
library(av)

setwd("C:/Users/user/Desktop/hrtf_project/datafiles")
getwd()

riverSound <- ('C:/Users/user/Desktop/hrtf_project/datafiles/river-6.wav')


# Show some info about audio file
av_media_info(riverSound)

# Read 5 sec of data and directly transform to frequency
fft_data <- read_audio_fft(riverSound, end_time = 5.0)
dim(fft_data)

# Plot the spectrogram
plot(fft_data)

pcm_data <- read_audio_bin(riverSound, channels = 1, end_time = 2.0)
plot(pcm_data, type = 'l')


# Create new audio file with first 5 sec
av_audio_convert(riverSound, 'x.mp3', total_time = 5)
av_spectrogram_video('x.mp3', output = 'spectrogramX.mp4', width = 1280, height = 720, res = 144)


#Spectrogram----nature-sound-(before-with dog and bird sounds)----------------------------------------------------------------

library(av)

setwd("C:/Users/user/Desktop/hrtf_project/datafiles")
getwd()

natureSoundBefore <- ('C:/Users/user/Desktop/hrtf_project/datafiles/naturesound-before(mp3).mp3')
summary(naturesoundBefore)

naturesound_before = readWave(natureSoundBefore)


# Read 5 sec of data and directly transform to frequency
fft_data <- read_audio_fft(naturesound_before, end_time = 5.0)

# Plot the spectrogram
plot(fft_data)


#Spectrogram----nature-sound-(after-without dog sound)------------------------------------------------------------------------

library(av)

setwd("C:/Users/user/Desktop/hrtf_project/datafiles")
getwd()

natureSoundAfter <- ('C:/Users/user/Desktop/hrtf_project/datafiles/naturesound-after(mp3).mp3')
summary(natureSoundAfter)

naturesound_after = readWave(natureSoundBefore)

#Read 5 sec of data and directly transform to frequency
fft_data <- read_audio_fft(naturesound_after, end_time = 5.0)

#Plot the spectrogram
plot(fft_data)


#deneme------------------------------------------------

library(seewave)

# File path
file_path <- "C:/Users/user/Desktop/hrtf_project/datafiles/river-6.wav"

# Load the audio file
audio <- readWave(file_path)

# Spectrogram parameters
f <- 22050
ovlp <- 50
hoff <- 0
voff <- 2
border_color <- "#00000075"

# Create waterfall display
wf(audio, f = f, ovlp = ovlp, hoff = hoff, voff = voff, border = border_color)


##############################################################################


#true 3D using rgl
download.file(
  "http://sueur.jerome.perso.neuf.fr/
WebPage_Sounds/E_chopardi_whistle.wav",
  destfile="cock.wav")
cock<-loadSample("cock.wav")
spectro3D(cock,
          f=22050,wl=490,
          ovlp=85,zp=6,maga=4,
          palette=spectro.colors)

# deneme2------------------------------------------------------------------------------------------------------------
#Load required libraries

library(signal)
library(rgl)

# File path
file_path <- "C:/Users/user/Desktop/hrtf_project/datafiles/river-6.wav"

# Load the audio file
audio <- readWave(file_path)

# Spectrogram parameters
f <- 22050
wl <- 490
ovlp <- 85
zp <- 6

# Create spectrogram
spec <- spectrogram(audio@left, f = f, w = wl, overlap = ovlp, n = zp * wl, plot = FALSE)

# Convert spectrogram to dB scale
spec_db <- 10 * log10(abs(spec$Sxx))

# Plot the 3D spectrogram using rgl
rgl.surface(x = spec$t, y = spec$f, z = spec_db, color = heat.colors(256), front="lines")
rgl.viewpoint(theta = 45, phi = 30, fov = 60, zoom = 0.75)

# Add labels
rgl.texts(x = range(spec$t), y = max(spec$f), z = max(spec_db),
          texts = c("Time", "Frequency", "Amplitude (dB)"))



#--------------------------------------------------------------------------------------------------------------------

data(tico)
wf(tico,f=22050)
# changing the display parameters
jet.colors <- colorRampPalette(c("blue", "green"))
wf(tico,f=22050, hoff=0, voff=2, col=jet.colors, border = NA)
# matrix input instead of a time wave and transparent lines display
m <- numeric()
for(i in seq(-pi,pi,len=40)) {m <- cbind(m,10*(sin(seq(0,2*pi,len=100)+i)))}
wf(x=m, lines=TRUE, col="#0000FF50",xlab="Time", ylab="Amplitude",
   main="waterfall display")



############deneme

library(seewave)

file_path <- "C:/Users/user/Desktop/hrtf_project/datafiles/river-6.wav"

audio <- readWave(file_path)

# Create a waterfall display
wf(audio, f = 22050)

jet.colors <- colorRampPalette(c("blue", "green"))
wf(audio, f = 22050, hoff = 0, voff = 2, col = jet.colors, border = NA)

# Matrix input instead of a time wave and transparent lines display
m <- numeric()
for (i in seq(-pi, pi, len = 40)) {
  m <- cbind(m, 10 * (sin(seq(0, 2 * pi, len = 100) + i)))
}
wf(x = m, lines = TRUE, col = "#0000FF50", xlab = "Time", ylab = "Amplitude", main = "Waterfall Display")



#Oscillogram----------------------------------------------------------------------------------------------------

# Load required libraries
library(seewave)

# Specify the path to your audio file
file_path <- "C:/Users/user/Desktop/hrtf_project/datafiles/naturesound-before"

# Read the audio file
audio_data <- readWave(file_path)


# Print the sampling frequency
print(audio_data@samp.rate)

# Plot the oscillogram
oscillo(audio_data, f = 48000)


oscillo(audio_data, f = 48000, k = 4, j = 1, title = TRUE, colwave = "black",
        coltitle = "yellow", collab = "red", colline = "white",
        colaxis = "blue", coly0 = "grey50")




#------deneme----------Simple oscillogram-------------------------------------------------------------------------------

oscillo(tico,f=22050)

#Multi-frame oscillogram
oscillo(tico,f=22050,k=2,j=2)

#Oscillogram and enveloppe
oscillo(tico,f=22050)
par(new=TRUE)
env(tico,f=22050,msmooth=c(20,0),colwave=2)


#Echo-Generating-----------------------------------------------------------------------------------------------------------

library(signal)
library(tuneR)

# Load signal
audio_file <- 'C:/Users/user/Desktop/hrtf_project/datafiles/boneyM-mabaker.wav'
y <- readWave(audio_file)

# Sample rate
Fs <- y@samp.rate

# Echo parameters
alpha <- 0.9
D <- 4196

# Filter parameters
b <- c(1, rep.int(0, times = D), alpha)

# Generate sound plus its echo
x <- signal::filter(b = b, a = 1, x = y@left)

# Play sound with echo
play(x, rate = Fs)




