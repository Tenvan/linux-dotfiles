#$env:MY_THEME = 'slimfat'
#$env:MY_THEME = 'slim'
$env:MY_THEME = 'pixelrobots'

oh-my-posh init pwsh --config "https://raw.githubusercontent.com/JanDeDobbeleer/oh-my-posh/main/themes/$env:MY_THEME.omp.json" | Invoke-Expression
