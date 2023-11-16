library(tuneR)
library(ggplot2)
library(pracma)
# Wczytaj plik WAV
sciezka_do_pliku <- "C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/src/1.wav"
dane_audio <- readWave("C:/Users/kubal/OneDrive/Dokumenty/Github/NoiseFilterR/src/fish.wav")
# Dokonaj FFT
s1<-dane_audio@left
s2 <- s1 / 2^(dane_audio@bit -1)
fhat <- fft(s2)
# Oblicz gęstość mocy (Power Spectral Density - PSD)
Pds <- Re(fhat*Conj(fhat)/(length(dane_audio)))
indeks_najwiekszej<- which.max(Pds)
frequency1 <- as.list(seq(0, (length(Pds) - 1)) * 44100 / length(Pds))
print(frequency1[indeks_najwiekszej])
prosze <- Pds < 4
Fhatclean=prosze*fhat
Pdsclean<- Re(Fhatclean*Conj(Fhatclean)/(length(dane_audio)))
# Stwórz ramkę danych do wykresu
df <- data.frame(frequency = seq(0, (length(Pdsclean) - 1) ) * 44100 / length(Pdsclean),
                 PSD = Pds[1:(length(Pdsclean))])
# Utwórz wykres

ggplot(df, aes(x = frequency, y = Pdsclean)) +
  geom_line() +
  labs(title = "Power Spectral Density (PSD) KURAS",
       x = "Frequency (Hz)",
       y = "PSD") +
  theme_minimal()