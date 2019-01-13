/*
Copyright 2015,2016,2017,2018,2019 Institut National de la Recherche Agronomique
and Montpellier SupAgro.

This file is part of PlantBreedGame.

PlantBreedGame is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

PlantBreedGame is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with PlantBreedGame.  If not, see
<http://www.gnu.org/licenses/>.
*/


// Function to indicate if the shiny server is busy or not

var timeLastMessage = new Date().getTime()

Shiny.addCustomMessageHandler("serverTic",function(message){
    timeLastMessage = new Date().getTime()
});

setInterval(function(){
  var elements = document.getElementsByClassName("serverIndicator")
  var tnow = new Date().getTime()
  for (var i = 0; i < elements.length; i++) {
    element = elements[i]
    var content = element.getElementsByTagName("h3")[0]

    if (tnow - timeLastMessage > 500 && content.innerHTML!="BUSY") {
      element.style.backgroundColor="#ff3333" //red
    content.innerHTML = "BUSY"

  } else if(tnow - timeLastMessage <= 500  && content.innerHTML!="FREE"){
      element.style.backgroundColor="#009933" //green
      content.innerHTML = "FREE"
    }

  }
},100)








/*

setInterval(function(){
  var elements = document.getElementsByClassName("serverIndicator")

  for (var i = 0; i < elements.length; i++) {
    element = elements[i]
    var content = element.getElementsByTagName("h3")[0]

    if ($('html').attr('class')=='shiny-busy' && content.innerHTML!="BUSY") {
    console.log("BUSY")
      element.style.backgroundColor="#ff3333" //red
    content.innerHTML = "BUSY"

    } else if($('html').attr('class')!='shiny-busy' && content.innerHTML!="FREE"){
      console.log("FREE")
      element.style.backgroundColor="#009933" //green
      content.innerHTML = "FREE"
    }

  }
},100)
*/
