library(tuneR)
library(ggplot2)
library(pracma)
# Wczytaj plik WAV
dane_audio <- readWave("C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/src/kwiatuszek.wav")
fs<-dane_audio@samp.rate
amp=0.2
f1=2000
BW=30
# Dokonaj FFT
s1<-dane_audio@left
audio1<-normalize(dane_audio)
s2 <- audio1@left
dane_audio3<-audio1
timeValues<-(1:length(s2)) /fs
noise<- amp * sin(2 * pi * f1 * timeValues)
s3<-s2+noise
dane_audio3@left<-s3
dane_audio3<-normalize(dane_audio3,unit = "16")
writeWave(dane_audio3,"C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/out/test.wav")
fhat <- fft(s3)
# Oblicz gęstość mocy (Power Spectral Density - PSD)
Pds <- Re(fhat*Conj(fhat)/(length(dane_audio)))
indeks_najwiekszej<- which.max(Pds)
if (indeks_najwiekszej>(length(Pds)/2)) {
  indeks_najwiekszej<-(length(Pds))-indeks_najwiekszej
}
frequency1 <- as.list(seq(0, (length(Pds) - 1)) * 44100 / length(Pds))
print(frequency1[indeks_najwiekszej])
prosze<-logical(length(s3))
for (i in 0:length(prosze)) {
  if(i>(indeks_najwiekszej-30)&&(i<indeks_najwiekszej+30)){
    prosze[i]=FALSE
  } else if(i>(length(s3)- indeks_najwiekszej-30)&&(i<length(s3)- indeks_najwiekszej+30)){
    prosze[i]=FALSE
  }else{
    prosze[i]=TRUE
  }
}
Fhatclean=prosze*fhat
Pdsclean<- Re(Fhatclean*Conj(Fhatclean)/(length(dane_audio)))
# Stwórz ramkę danych do wykresu
df <- data.frame(frequency = seq(0, (length(Pdsclean) - 1) ) * 44100 / length(Pdsclean),
                 PSD = Pds[1:(length(Pdsclean))])
# Utwórz wykres

ggplot(df, aes(x = frequency, y = Pdsclean)) +
  geom_line() +
  labs(title = "Power Spectral Density (PSD) OutputSignal",
       x = "Frequency (Hz)",
       y = "PSD") +
  theme_minimal()
ggsave("C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/plt/plot1.jpg")
output=Re(ifft(Fhatclean))
dane_audio@left<-output
dane_audio<-normalize(dane_audio,unit = "16")
writeWave(dane_audio,"C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/out/blagam.wav")