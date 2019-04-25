FC := gfortran
SRCDIR := src
DESTDIR := bin

SRC := $(SRCDIR)/mucg83.f
TARGET := $(DESTDIR)/mucg83

$(TARGET):
	$(FC) -o $@ $(SRC)

clean: $(TARGET)
	rm -f $^