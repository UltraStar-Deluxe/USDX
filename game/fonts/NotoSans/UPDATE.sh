#!/bin/bash

set -euo pipefail

# List of font files to download
fonts=(
  NotoSans-Bold.ttf
  NotoSans-Italic.ttf
  NotoSans-Regular.ttf
  NotoSansArabic-Bold.ttf
  NotoSansArabic-Regular.ttf
  NotoSansArmenian-Bold.ttf
  NotoSansArmenian-Regular.ttf
  NotoSansBengali-Bold.ttf
  NotoSansBengali-Regular.ttf
  NotoSansDevanagari-Bold.ttf
  NotoSansDevanagari-Regular.ttf
  NotoSansGeorgian-Bold.ttf
  NotoSansGeorgian-Regular.ttf
  NotoSansGujarati-Bold.ttf
  NotoSansGujarati-Regular.ttf
  NotoSansGurmukhi-Bold.ttf
  NotoSansGurmukhi-Regular.ttf
  NotoSansHebrew-Bold.ttf
  NotoSansHebrew-Regular.ttf
  NotoSansKannada-Bold.ttf
  NotoSansKannada-Regular.ttf
  NotoSansLao-Bold.ttf
  NotoSansLao-Regular.ttf
  NotoSansMalayalam-Bold.ttf
  NotoSansMalayalam-Regular.ttf
  NotoSansMyanmar-Bold.ttf
  NotoSansMyanmar-Regular.ttf
  NotoSansOriya-Bold.ttf
  NotoSansOriya-Regular.ttf
  NotoSansSymbols-Bold.ttf
  NotoSansSymbols-Regular.ttf
  NotoSansSymbols2-Regular.ttf
  NotoSansTamil-Bold.ttf
  NotoSansTamil-Regular.ttf
  NotoSansTelugu-Bold.ttf
  NotoSansTelugu-Regular.ttf
  NotoSansThai-Bold.ttf
  NotoSansThai-Regular.ttf
)

count=0

for font in "${fonts[@]}"; do
  echo "Downloading $font..."

  family="${font%%-*}"
  url="https://cdn.jsdelivr.net/gh/notofonts/notofonts.github.io/fonts/${family}/hinted/ttf/${font}"

  if curl -fSL "$url" -o "$font"; then
    ((count++))
  else
    echo "❌ Failed to download: $font"
  fi
done

echo "✅ Successfully downloaded $count font files!"