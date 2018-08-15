Remove-Item result.zip
New-Item -Path .temp -ItemType directory
py py/js script.js | Set-Content '.temp\script.js'
py py/html index.html | Set-Content '.temp\index.html'
Compress-Archive -Path .temp -DestinationPath result.zip
Remove-Item .temp -Recurse -Force
$size=(Get-Item 'result.zip').length
Write-Output "$size bytes of 13312: [int]($size*100/13312)%"
