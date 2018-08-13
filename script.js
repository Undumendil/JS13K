const canvas = document.getElementById("canvas")
const context = canvas.getContext("2d")
context.font = "40px Comic Sans MS"
context.textAlign = "center"
context.fillText("Hello world!", canvas.width / 2, canvas.height / 2)
