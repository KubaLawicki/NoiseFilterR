#importowanie bibliotek
library(tuneR)
library(ggplot2)
library(pracma)

#Funkcja rysujaca PSD
PlotAndSavePSD <- function( PDSclean, nazwa_pliku) {
 #frame dla danych 
  df <- data.frame(frequency = seq(0, (length(PDSclean) - 1) ) * 44100 / length(PDSclean),
                   PDSclean = PDSclean[1:(length(PDSclean))])
  
#tworzenie wykresu
  wykres <- ggplot(df, aes(x = frequency, y = PDSclean)) +
    geom_line() +
    labs(title = "Power Spectral Density (PSD) OutputSignal",
         x = "Frequency (Hz)",
         y = "PSD") +
    theme_minimal()
  ggsave(nazwa_pliku, plot = wykres, width = 8, height = 6, dpi = 300)
}

# Wczytaj plik WAV
dane_audio <- readWave("C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/src/aplauz.wav")

#stale szumu
fs<-dane_audio@samp.rate
amp<-1
f1<-runif(1,200,10000)   

#przejscie z formatu 16 bitowego na 1
audio1<-normalize(dane_audio)
s2 <- audio1@left

#PSD na pliku wejscia
fhatinput<-fft(s2)
PDSinput <- Re(fhatinput*Conj(fhatinput)/(length(dane_audio)))

#tworzenie wykresu PSD sygnalu wejsciowego
PlotAndSavePSD(PDSinput,"C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/plt/PSDInput.jpg")
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

#tworzenie wykresu PSD przed filtracja
PlotAndSavePSD(PDS,"C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/plt/PSDNoise.jpg")
#znalezienie najwiekszej wartosci PDS oraz 
indeks_najwiekszej<- which.max(PDS)
if (indeks_najwiekszej>(length(PDS)/2)) {
  indeks_najwiekszej<-(length(PDS))-indeks_najwiekszej
}

#sprawdzenie czy na dobrej czestotliwosci wykryto szum
frequency1 <- as.list(seq(0, (length(PDS) - 1)) * 44100 / length(PDS))
print(frequency1[indeks_najwiekszej])
print(f1)

#stworzenie wspolczynnika filtra notch w dziedzinie czestotliwosci
notch_filter_index<-logical(length(s3))
for (i in 0:length(notch_filter_index)) {
  if(i>(indeks_najwiekszej-100)&&(i<indeks_najwiekszej+100)){
    notch_filter_index[i]=FALSE
  } else if(i>(length(s3)- indeks_najwiekszej-100)&&(i<length(s3)- indeks_najwiekszej+100)){
    notch_filter_index[i]=FALSE
  }else{
    notch_filter_index[i]=TRUE
  }
}

#Filtracja metoda szybkiego splotu
Fhatclean=notch_filter_index*fhat

# Obliczenie gęstości mocy na sygnale przefiltrowanym(Power Spectral Density - PSD)
PDSclean<- Re(Fhatclean*Conj(Fhatclean)/(length(dane_audio)))

#tworzenie wykresu PSD po filtracji
PlotAndSavePSD(PDSclean,"C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/plt/PSDClean.jpg")

#odwrotna transformata fouriera, czysty sygnal
output=Re(ifft(Fhatclean))

#zapisanie czystego sygnalu
dane_audioout<-dane_audio
dane_audioout@left<-output
dane_audioout<-normalize(dane_audioout,unit = "16")
writeWave(dane_audioout,"C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/out/CleanSound.wav")