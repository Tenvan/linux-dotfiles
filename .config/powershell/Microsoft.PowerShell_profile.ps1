# Aktivate Powershell Git Modul
#Import-Module posh-git

# Set Powershell Prompt Style
$Env:POSH_THEME = "~/.config/powershell/xsession.omp.json" 
# Oder tokyo,peru,onehalf.minimal

echo "Used Theme: $Env:POSH_THEME"
oh-my-posh init pwsh --config "$Env:POSH_THEME" | Invoke-Expression

# Shows navigable menu of all options when hitting Tab
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete

# Autocompletion for arrow keys
Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward
