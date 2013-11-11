
# "dortmund" shiny app

- Firefox can only play OGG files!

- Somehow, there is a problem when using the <audio><source></source></audio>
  tags and updating the src attribute of the source tag. Then, the 
  player does not recognize the change. I don't know how to solve it,
  so I just update the src attribute of the audio tag.

- jQuery changes the DOM (local structure in memory) and therefore it
  is only visible in the developer tools of a browser, and not via the
  simple source code viewer.

- The MP3 files have to be in the www directory.