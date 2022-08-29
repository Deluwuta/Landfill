# Comando pa sacar el link quality de la wifi

echo "$(tail -n+3 /proc/net/wireless | awk '{ print $3 }' | cut -d. -f1)"

# 1ª parte: Saca la línea que quiero
# 2ª parte: Saca la parte que quiero
# 3ª parte: Quita un punto '-'
