#importowanie bibliotek
library(tuneR)
library(ggplot2)
library(pracma)


# Wczytaj plik WAV
dane_audio <- readWave("C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/src/aplauz.wav")

#stale szumu
fs<-dane_audio@samp.rate
amp<-1
f1<-runif(1,100,10000)   

#przejscie z formatu 16 bitowego na 1
audio1<-normalize(dane_audio)
s2 <- audio1@left

#stworzenie szumu
timeValues<-(1:length(s2)) /fs
noise<- amp * sin(2 * pi * f1 * timeValues)
s3<-s2+noise

##przejscie z formatu 16 bitowego na 1 dla danych z szumem
dane_audio3<-audio1
dane_audio3@left<-s3
dane_audio3<-normalize(dane_audio3,unit = "16")

#zapisanie pliku wav z szumem
writeWave(dane_audio3,"C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/out/AudioWithNoise.wav")

# Przeprowadzenie FFT 
fhat <- fft(s3)

# Obliczenie gęstości mocy (Power Spectral Density - PSD)
PDS <- Re(fhat*Conj(fhat)/(length(dane_audio)))

#znalezienie najwiekszej wartosci PDS oraz 
indeks_najwiekszej<- which.max(PDS)
if (indeks_najwiekszej>(length(PDS)/2)) {
  indeks_najwiekszej<-(length(PDS))-indeks_najwiekszej
}

#sprawdzenie czy na dobrej czestotliwosci wykryto szum
frequency1 <- as.list(seq(0, (length(PDS) - 1)) * 44100 / length(PDS))
print(frequency1[indeks_najwiekszej])
print(f1)

#stworzenie wspolczynnika filtra noc w dziedzinie czestotliwosci
notch_filter_index<-logical(length(s3))
for (i in 0:length(notch_filter_index)) {
  if(i>(indeks_najwiekszej-50)&&(i<indeks_najwiekszej+50)){
    notch_filter_index[i]=FALSE
  } else if(i>(length(s3)- indeks_najwiekszej-50)&&(i<length(s3)- indeks_najwiekszej+50)){
    notch_filter_index[i]=FALSE
  }else{
    notch_filter_index[i]=TRUE
  }
}

#Filtracja metoda szybkiego splotu
Fhatclean=notch_filter_index*fhat

# Obliczenie gęstości mocy na sygnale przefiltrowanym(Power Spectral Density - PSD)
PDSclean<- Re(Fhatclean*Conj(Fhatclean)/(length(dane_audio)))

#Stworzenie wykresu
# Stwórz ramkę danych do wykresu
df <- data.frame(frequency = seq(0, (length(PDSclean) - 1) ) * 44100 / length(PDSclean),
                 PSD = PDS[1:(length(PDSclean))])

# Utwórz wykres
ggplot(df, aes(x = frequency, y = PDSclean)) +
  geom_line() +
  labs(title = "Power Spectral Density (PSD) OutputSignal",
       x = "Frequency (Hz)",
       y = "PSD") +
  theme_minimal()
ggsave("C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/plt/plot1.jpg")

#odwrotna transformata fouriera i czysty sygnal
output=Re(ifft(Fhatclean))

#zapisanie czystego sygnalu
dane_audioout<-dane_audio
dane_audioout@left<-output
dane_audioout<-normalize(dane_audioout,unit = "16")
writeWave(dane_audioout,"C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/out/CleanSound.wav")