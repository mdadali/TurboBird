# 1. Definisco i percorsi esatti (con le virgolette per gestire gli spazi nel nome delle cartelle)
IMG_SORGENTE="/Volumes/TransDati/projects Lazarus/TurboBird_for_laz_48-main 3/src/TurboBird_256x256.png"
CARTELLA_DESTINAZIONE="/Volumes/TransDati/projects Lazarus/TurboBird_for_laz_48-main 3/src/macos_icon"
CARTELLA_TEMP="$CARTELLA_DESTINAZIONE/TurboBird.iconset"

# 2. Creo le cartelle
mkdir -p "$CARTELLA_TEMP"

# 3. Genero tutte le dimensioni necessarie usando sips
sips -z 16 16     "$IMG_SORGENTE" --out "$CARTELLA_TEMP/icon_16x16.png"
sips -z 32 32     "$IMG_SORGENTE" --out "$CARTELLA_TEMP/icon_32x32.png"
sips -z 128 128   "$IMG_SORGENTE" --out "$CARTELLA_TEMP/icon_128x128.png"
sips -z 256 256   "$IMG_SORGENTE" --out "$CARTELLA_TEMP/icon_256x256.png"
sips -z 512 512   "$IMG_SORGENTE" --out "$CARTELLA_TEMP/icon_512x512.png"

sips -z 32 32     "$IMG_SORGENTE" --out "$CARTELLA_TEMP/icon_16x16@2x.png"
sips -z 64 64     "$IMG_SORGENTE" --out "$CARTELLA_TEMP/icon_32x32@2x.png"
sips -z 256 256   "$IMG_SORGENTE" --out "$CARTELLA_TEMP/icon_128x128@2x.png"
sips -z 512 512   "$IMG_SORGENTE" --out "$CARTELLA_TEMP/icon_256x256@2x.png"
sips -z 1024 1024 "$IMG_SORGENTE" --out "$CARTELLA_TEMP/icon_512x512@2x.png"

# 4. Assemblo il file .icns definitivo
iconutil -c icns "$CARTELLA_TEMP" -o "$CARTELLA_DESTINAZIONE/TurboBird.icns"

# 5. Elimino la cartella temporanea con le singole PNG
rm -rf "$CARTELLA_TEMP"

echo "Fatto! Il file TurboBird.icns è pronto in: $CARTELLA_DESTINAZIONE"
