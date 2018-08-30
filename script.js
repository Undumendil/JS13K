const gl = space.getContext("webgl")
if (!gl)
	throw new Error("Could not initialize WebGL")

gl.clearDepth(1)
gl.enable(gl.DEPTH_TEST)
gl.depthFunc(gl.LEQUAL)
gl.disable(gl.BLEND)

class Model {
	constructor() {
		this.translation = { x: 0, y: 0, z: 0 }
		this.rotation = { y: 0, x: 0, z: 0 }
		this.offset = { x: 0, y: 0, z: 0 }
		this.scale = { x: 1, y: 1, z: 1 }
	}

	total() {
		return scale(this.scale.x, this.scale.y, this.scale.z)
				.x(translate(this.offset.x, this.offset.y, this.offset.z))
				.x(rotate(this.rotation.y, this.rotation.x, this.rotation.z))
				.x(translate(this.translation.x, this.translation.y, this.translation.z))
	}

	inverse() {
		return translate(-this.translation.x, -this.translation.y, -this.translation.z)
				.x(rotate(-this.rotation.y, -this.rotation.x, -this.rotation.z, true))
				.x(translate(-this.offset.x, -this.offset.y, -this.offset.z))
				.x(scale(1 / this.scale.x, 1 / this.scale.y, 1 / this.scale.z))
	}
}

Array.prototype.flatten = function(){
	return [].concat(...this.map(x => Array.isArray(x) ? x.flatten() : x))
}

class Mesh extends Model {
	constructor(trianglesColor, linesColor, points, triangles) {
		super()
		this.children = []
		this.linesColor = linesColor
		this.trianglesColor = trianglesColor
		this.cachedPoints = []
		points.forEach(x => this.cachedPoints.push(x))
		this.points = createArrayBuffer(points)
		this.trianglesIDs = createArrayBuffer(triangles, true)
		this.cachedLines = calculateLinesIDs(triangles)
		this.linesIDs = createArrayBuffer(this.cachedLines, true)
		this.trianglesLength = triangles.length
		this.linesLength = this.cachedLines.length
	}

	update(pointsIDs, coords){
		for (let idOfID = 0; idOfID < pointsIDs.length; idOfID++){
			modifyArrayBuffer(this.points, pointsIDs[idOfID] * 3, coords.slice(idOfID * 3, idOfID * 3 + 3))
			this.cachedPoints[pointsIDs[idOfID] * 3] = coords[idOfID * 3]
			this.cachedPoints[pointsIDs[idOfID] * 3 + 1] = coords[idOfID * 3 + 1]
			this.cachedPoints[pointsIDs[idOfID] * 3 + 2] = coords[idOfID * 3 + 2]
		}
	}

	updateX(pointID, x){
		modifyArrayBuffer(this.points, pointID * 3, [x])
		this.cachedPoints[pointID * 3] = x
	}

	updateY(pointID, y){
		modifyArrayBuffer(this.points, pointID * 3 + 1, [y])
		this.cachedPoints[pointID * 3 + 1] = y
	}

	updateZ(pointID, z){
		modifyArrayBuffer(this.points, pointID * 3 + 2, [z])
		this.cachedPoints[pointID * 3 + 2] = z
	}

	read(pointID){
		return this.cachedPoints.slice(pointID * 3, pointID * 3 + 3)
	}

	readX(pointID){
		return this.cachedPoints[pointID * 3]
	}

	readY(pointID){
		return this.cachedPoints[pointID * 3 + 1]
	}

	readZ(pointID){
		return this.cachedPoints[pointID * 3 + 2]
	}

	render(properties, parentModel) {
		const model = this.total().x(parentModel)
		this.children.forEach(it => it.render(properties, model))
		gl.uniformMatrix4fv(properties.uModel, false, model)

		properties.aPosition.set(this.points, 3)
		gl.uniform1f(properties.uTimeColor, WORLD.timeColor)
		gl.uniform1f(properties.uForward, 0.0)
		gl.uniform1i(properties.uColor, this.trianglesColor)
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.trianglesIDs)
		gl.drawElements(gl.TRIANGLES, this.trianglesLength, gl.UNSIGNED_BYTE, 0)

		gl.uniform1f(properties.uTimeColor, WORLD.timeColor)
		gl.uniform1f(properties.uForward, 0.001)
		gl.uniform1i(properties.uColor, this.linesColor)
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.linesIDs)
		gl.drawElements(gl.LINES, this.linesLength, gl.UNSIGNED_BYTE, 0)
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, null)
	}
}

class Attribute {
	constructor(program, name) {
		this.attrib = gl.getAttribLocation(program, name)
	}

	set(buffer, size) {
		gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
		gl.vertexAttribPointer(this.attrib, size, gl.FLOAT, false, 0, 0)
		gl.bindBuffer(gl.ARRAY_BUFFER, null)
		return this
	}

	enable() {
		gl.enableVertexAttribArray(this.attrib)
		return this
	}

	disable() {
		gl.disableVertexAttribArray(this.attrib)
		return this
	}
}

class Random {
	constructor(seed = 123456789){
		this.seed = seed
		this.something = 987654321
		this.mask = 0xffffffff
	}
	get(){
		this.something = (36969 * (this.something & 65535) + (this.something >> 16)) & this.mask
		this.seed = (18000 * (this.seed & 65535) + (this.seed >> 16)) & this.mask
		return (((this.something << 16) + this.seed) & this.mask) / 4294967296 + 0.5
	}
	setSeed(seed){
		this.seed = seed
		this.something = 987654321
	}
	roll(probability){
		return this.get() < probability
	}
}

Array.prototype.x = function(m) {
	const out = []
	for (let i = 0; i < 4; i++)
		for (let j = 0; j < this.length / 4; j++) {
			out[j * 4 + i] = 0
			for (let k = 0; k < 4; k++)
				out[j * 4 + i] += m[i + k * 4] * this[j * 4 + k]
		}
	return out
}

Array.prototype.areThereAnyOf = function(tileArray, cam){
	for (tile in tileArray){
		let tData = tile.split(",")
		if (this.indexOf(tile.add(renderCamera.toString().mul(-1))) != -1 && tileArray[tile] > 0)
			return true
	}
	return false
}

Array.prototype.fill = function(generator){
	this.length = 0
	let params = Array.prototype.slice.apply(arguments).slice(1)
	let tempTiles = []
	generator.apply(null, [this, tempTiles, params, 0, 0])
	while (tempTiles.length > 0){
		let tile = tempTiles.shift()
		generator.apply(null, [this, tempTiles, params, tile.x(), tile.y()])
	}
	return this
}

Array.prototype.isInside = function(tile){
	return this.indexOf(tile) != -1
}

String.prototype.up = function(){
	let tData = this.split(",")
	return (+tData[0] + +tData[1]) % 2 != 0
}

String.prototype.add = function(){
	let tData = this.split(",")
	if (arguments.length == 1){
		if (typeof arguments[0] == "object"){
			return (+tData[0] + arguments[0][0]) + "," + (+tData[1] + arguments[0][1])
		} else {
			let uData = arguments[0].split(",")
			return (+tData[0] + +uData[0]) + "," + (+tData[1] + +uData[1])
		}
	}
	return (+tData[0] + arguments[0]) + "," + (+tData[1] + arguments[1])
}

String.prototype.x = function(){
	return +this.split(",")[0]
}

String.prototype.y = function(){
	return +this.split(",")[1]
}

String.prototype.len = function(){
	let tData = this.split(",")
	return Math.sqrt(+tData[0] * +tData[0] + +tData[1] * +tData[1])
}

String.prototype.mul = function(multiplier){
	let tData = this.split(",")
	if (typeof multiplier == "string")
		return +tData[0] * multiplier.x + +tData[1] * multiplier.y
	return (+tData[0] * multiplier) + "," + (+tData[1] * multiplier)
}

String.prototype.setLength = function(length){
	let tData = this.split(",")
	if (+tData[0] == 0 && +tData[1] == 0)
		return this
	return this.mul(length / this.len())
}

String.prototype.toWorldCoordinate = function(){
	let tData = this.split(",")
	return ((+tData[0] / 2) + "," + (sqrt3 / 3 + +tData[1] * sqrt3 / 2 - sqrt3 / 6 * this.up())).mul(CHUNK_SIDE)
}

const surroundings = (center) => center.up() ?
					 [ "1,0", "1,1", "0,1", "-1,1", "-1,0", "-2,0", "-2,-1", "-1,-1", "0,-1", "1,-1", "2,-1", "2,0" ] :
					 [ "1,0", "2,0", "2,1",  "1,1",  "0,1", "-1,1", "-2,1",  "-2,0", "-1,0", "-1,-1", "0,-1", "1,-1" ]
const goodSurroundings = (center) => center.up() ?
						 [ "1,0", "-1,0", "0,-1" ] :
						 [ "1,0", "-1,0", "0,1" ]

function inside(up, vec){
	vec = vec.add(0, -2 * vec.y())
	if (up){
		vec = vec.add(CHUNK_SIDE / 2, CHUNK_SIDE * sqrt3 / 6)
		let B = "0,0".add(CHUNK_SIDE, 0)
		let C = "0,0".add(CHUNK_SIDE / 2, CHUNK_SIDE * sqrt3 / 2)
		let m = (vec.x() * B.y() - vec.y() * B.x()) / (C.x() * B.y() - B.x() * C.y())
		if (m >= 0 && m <= 1){
			let l = (vec.x() - m * C.x()) / B.x()
			if (l >= 0 && (m + l) <= 1)
				return true
		}
	} else {
		vec = vec.add(CHUNK_SIDE / 2, -CHUNK_SIDE * sqrt3 / 6)
		let B = "0,0".add(CHUNK_SIDE, 0)
		let C = "0,0".add(CHUNK_SIDE / 2, -CHUNK_SIDE * sqrt3 / 2)
		let m = (vec.x() * B.y() - vec.y() * B.x()) / (C.x() * B.y() - B.x() * C.y())
		if (m >= 0 && m <= 1){
			let l = (vec.x() - m * C.x()) / B.x()
			if (l >= 0 && (m + l) <= 1)
				return true
		}
	}
	return false
}

function mat(x = 0, y = 0, z = 0, a = 1, b = 1, c = 1, d = 0, e = 1) {
	return [
		a, 0, 0, 0,
		0, b, 0, 0,
		0, 0, c, d,
		x, y, z, e
	]
}

const translate = (x, y, z) => mat(x, y, z)
const scale = (x, y, z) => mat(0, 0, 0, x, y, z)
const ortho = (aspect, distance) => mat(0, 0, 0, 1 / aspect, 1, 1 / distance)
const deg = angle => angle * Math.PI / 180

function perspective(y, a, n, f) {
	const c = (f + n) / (f - n)
	const b = 2 * f * n / (n - f)
	const h = 1 / Math.tan(y / 2)
	const w = h / a
	return mat(0, 0, b, w, h, c, 1, 0)
}

function rotate(dy, dx, dz, inverse) {
	const rotY = [
		Math.cos(dy), 0, -Math.sin(dy), 0,
		0,            1, 0,             0,
		Math.sin(dy), 0, Math.cos(dy),  0,
		0,            0, 0,             1,
	]
	const rotX = [
		1, 0,             0,            0,
		0, Math.cos(dx),  Math.sin(dx), 0,
		0, -Math.sin(dx), Math.cos(dx), 0,
		0, 0,             0,            1
	]
	const rotZ = [
		Math.cos(dz),  Math.sin(dz), 0, 0,
		-Math.sin(dz), Math.cos(dz), 0, 0,
		0,             0,            1, 0,
		0,             0,            0, 1,
	]
	return inverse ? rotY.x(rotX).x(rotZ) : rotZ.x(rotX).x(rotY)
}

function compile(type, source) {
	let shader = gl.createShader(type)
	gl.shaderSource(shader, source)
	gl.compileShader(shader)

	if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
		console.error("ShaderProgram: could not compile " +
			(type == gl.VERTEX_SHADER ? "vertex" : "fragment") + " shader")
		console.error(gl.getShaderInfoLog(shader))
		shader = 0
	}

	return shader
}

function link(vertex, fragment) {
	let program = gl.createProgram()
	gl.attachShader(program, fragment)
	gl.attachShader(program, vertex)
	gl.linkProgram(program)

	 if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
		console.error("ShaderProgram: could not link program")
		console.error(gl.getProgramInfoLog(program))
		program = 0
	}

	return program
}

function createArrayBuffer(data, indexBuffer = false) {
	const buffer = gl.createBuffer()
	let type = indexBuffer ? gl.ELEMENT_ARRAY_BUFFER : gl.ARRAY_BUFFER
	gl.bindBuffer(type, buffer)
	gl.bufferData(type, indexBuffer ? new Uint8Array(data) : new Float32Array(data), gl.STATIC_DRAW);
	gl.bindBuffer(type, null)
	return buffer
}

function modifyArrayBuffer(buffer, offset, data) {
	const data32 = new Float32Array(data)
	gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
	gl.bufferSubData(gl.ARRAY_BUFFER, offset * 4, data32)
	gl.bindBuffer(gl.ARRAY_BUFFER, null)
	return buffer
}

function cache(container, target, a, b) {
	const ab = a + ':' + b
	const ba = b + ':' + a
	if (!container[ab] && !container[ba]) {
		container[ab] = container[ba] = true
		target.push(a)
		target.push(b)
	}
}

function calculateLinesIDs(trianglesIDs) {
	const found = {}
	const lines = []
	for (let i = 0; i < trianglesIDs.length; i += 3) {
		const a = trianglesIDs[i]
		const b = trianglesIDs[i + 1]
		const c = trianglesIDs[i + 2]
		cache(found, lines, a, b)
		cache(found, lines, b, c)
		cache(found, lines, a, c)
	}
	return lines
}

function range(lower, target, higher) {
	if (target < lower) return lower
	if (target > higher) return higher
	return target
}

function addRhombusTile(curArea, tempTiles, params, x, y){
	if (Math.abs(x) / 2 + Math.abs(y) <= params[0] && curArea.indexOf(x + "," + y) == -1){
		curArea.push(x + "," + y)
		for (tile of [[x + 1, y], [x - 1, y], [x, y + 1], [x, y - 1]])
			if (Math.abs(tile[0]) / 2 + Math.abs(tile[1]) <= params[0]
				&& curArea.indexOf(tile.toString()) == -1 && tempTiles.indexOf(tile.toString()) == -1)
				tempTiles.push(tile.toString())
	}
}

function addCircleTile(curArea, tempTiles, params, x, y){
	if (x * x / 3.4 + y * y <= params[0] * params[0] && curArea.indexOf(x + "," + y) == -1){
		curArea.push(x + "," + y)
		for (tile of [[x + 1, y], [x - 1, y], [x, y + 1], [x, y - 1]])
			if (tile[0] * tile[0] / 3.4 + tile[1] * tile[1] <= params[0] * params[0]
				&& curArea.indexOf(tile.toString()) == -1 && tempTiles.indexOf(tile.toString()) == -1)
				tempTiles.push(tile.toString())
	}
}

function addRectTile(curArea, tempTiles, params, x, y){
	if (Math.abs(x) <= params[0] / 2 && Math.abs(y) <= params[1] / 2 && curArea.indexOf(x + "," + y) == -1){
		curArea.push(x + "," + y)
		for (tile of [[x + 1, y], [x - 1, y], [x, y + 1], [x, y - 1]])
			if (Math.abs(x) <= params[0] / 2 && Math.abs(y) <= params[1] / 2
				&& curArea.indexOf(tile.toString()) == -1 && tempTiles.indexOf(tile.toString()) == -1)
				tempTiles.push(tile.toString())
	}
}

const MESH_VERTEX_SHADER = compile(gl.VERTEX_SHADER, `
	precision lowp float;
	attribute vec3 aPosition;
	uniform mat4 uModel;
	uniform mat4 uView;
	uniform mat4 uProjection;
	uniform float uForward;
	uniform vec3 uFogPivot;
	varying float vDistance;

	void main(void) {
		gl_Position = uProjection * uView * uModel * vec4(aPosition, 1.0);
		gl_Position.z -= uForward;
		vDistance = length((uModel * vec4(aPosition, 1.0)).xyz - uFogPivot);
	}
`)

const WATER_VERTEX_SHADER = compile(gl.VERTEX_SHADER, `
	precision lowp float;
	attribute vec3 aPosition;
	uniform mat4 uModel;
	uniform mat4 uView;
	uniform mat4 uProjection;
	uniform float uForward;
	uniform float uState;
	uniform vec3 uFogPivot;
	varying float vDistance;

	float distortion(vec3 p) {
		float wave = 5.0;
		float x = (p.x / wave + uState) * 2.0 * 3.14;
		float z = (p.z / wave + uState) * 2.0 * 3.14;
		return 0.1 * (sin(x) + cos(z));
	}

	void main(void) {
		vec4 globalPosition = uModel * vec4(aPosition, 1.0);
		globalPosition.y += distortion(globalPosition.xyz);
		gl_Position = uProjection * uView * globalPosition;
		gl_Position.z -= uForward;
		vDistance = length((uModel * vec4(aPosition, 1.0)).xyz - uFogPivot);
	}
`)

const FRAGMENT_SHADER = compile(gl.FRAGMENT_SHADER, `
	precision lowp float;
	uniform int uColor;
	uniform float uTimeColor;
	varying float vDistance;

	vec3 getColor(){
		if (uColor == 0) //WATER_TRIANGLES_COLOR
			return vec3(0.0, 1.0 - 0.8 * uTimeColor, 0.9 - 0.7 * uTimeColor);
		if (uColor == 1) //WATER_LINES_COLOR
			return vec3(0.0, 0.6 - 0.5 * uTimeColor, 0.5 - 0.4 * uTimeColor);
		if (uColor == 2) //ISLAND_TRIANGLES_COLOR
			return vec3(0.3 - 0.1 * uTimeColor, 0.8 - 0.2 * uTimeColor, 0.3 - 0.1 * uTimeColor);
		if (uColor == 3) //ISLAND_LINES_COLOR
			return vec3(0.2 - 0.1 * uTimeColor, 0.5 - 0.2 * uTimeColor, 0.2 - 0.1 * uTimeColor);
		if (uColor == 4) //BOAT_BODY_TRIANGLES_COLOR; BOAT_MAST_TRIANGLES_COLOR
			return vec3(0.6 - 0.3 * uTimeColor, 0.5 - 0.2 * uTimeColor, 0.4 - 0.2 * uTimeColor);
		if (uColor == 5) //BOAT_BODY_LINES_COLOR; BOAT_MAST_LINES_COLOR
			return vec3(0.5 - 0.3 * uTimeColor, 0.4 - 0.2 * uTimeColor, 0.3 - 0.1 * uTimeColor);
		if (uColor == 6) //BOAT_SAIL_TRIANGLES_COLOR
			return vec3(1.0 - 0.3 * uTimeColor, 1.0 - 0.3 * uTimeColor, 1.0 - 0.3 * uTimeColor);
		if (uColor == 7) //BOAT_SAIL_LINES_COLOR
			return vec3(0.6 - 0.2 * uTimeColor, 0.6 - 0.2 * uTimeColor, 0.6 - 0.2 * uTimeColor);
		if (uColor == 8) //ROCK_TRIANGLES_COLOR
			return vec3(0.6 - 0.4 * uTimeColor, 0.6 - 0.4 * uTimeColor, 0.6 - 0.4 * uTimeColor);
		if (uColor == 9) //ROCK_LINES_COLOR
			return vec3(0.5 - 0.4 * uTimeColor, 0.5 - 0.4 * uTimeColor, 0.5 - 0.4 * uTimeColor);
		return vec3(0.0, 0.0, 0.0);
	}

	void main(void) {
		float distance = 10.0 * 3.4 * (1.0 + uTimeColor);
		float state = max(0.0, (distance - vDistance) / distance);
		state *= state;
		gl_FragColor = vec4(getColor() * state + uTimeColor * (1.0 - state), 1.0);
	}
`)

const MESH_PROGRAM = link(MESH_VERTEX_SHADER, FRAGMENT_SHADER)
const MESH_PROGRAM_PROPERTIES = {}
MESH_PROGRAM_PROPERTIES.aPosition = new Attribute(MESH_PROGRAM, "aPosition").enable()
MESH_PROGRAM_PROPERTIES.uProjection = gl.getUniformLocation(MESH_PROGRAM, "uProjection")
MESH_PROGRAM_PROPERTIES.uTimeColor = gl.getUniformLocation(MESH_PROGRAM, "uTimeColor")
MESH_PROGRAM_PROPERTIES.uFogPivot = gl.getUniformLocation(MESH_PROGRAM, "uFogPivot")
MESH_PROGRAM_PROPERTIES.uForward = gl.getUniformLocation(MESH_PROGRAM, "uForward")
MESH_PROGRAM_PROPERTIES.uColor = gl.getUniformLocation(MESH_PROGRAM, "uColor")
MESH_PROGRAM_PROPERTIES.uModel = gl.getUniformLocation(MESH_PROGRAM, "uModel")
MESH_PROGRAM_PROPERTIES.uView = gl.getUniformLocation(MESH_PROGRAM, "uView")

const WATER_PROGRAM = link(WATER_VERTEX_SHADER, FRAGMENT_SHADER)
const WATER_PROGRAM_PROPERTIES = {}
WATER_PROGRAM_PROPERTIES.aPosition = new Attribute(WATER_PROGRAM, "aPosition").enable()
WATER_PROGRAM_PROPERTIES.uProjection = gl.getUniformLocation(WATER_PROGRAM, "uProjection")
WATER_PROGRAM_PROPERTIES.uTimeColor = gl.getUniformLocation(WATER_PROGRAM, "uTimeColor")
WATER_PROGRAM_PROPERTIES.uFogPivot = gl.getUniformLocation(WATER_PROGRAM, "uFogPivot")
WATER_PROGRAM_PROPERTIES.uForward = gl.getUniformLocation(WATER_PROGRAM, "uForward")
WATER_PROGRAM_PROPERTIES.uState = gl.getUniformLocation(WATER_PROGRAM, "uState")
WATER_PROGRAM_PROPERTIES.uColor = gl.getUniformLocation(WATER_PROGRAM, "uColor")
WATER_PROGRAM_PROPERTIES.uModel = gl.getUniformLocation(WATER_PROGRAM, "uModel")
WATER_PROGRAM_PROPERTIES.uView = gl.getUniformLocation(WATER_PROGRAM, "uView")

const CAMERA = new Model()
CAMERA.rotation.x = deg(20)
CAMERA.offset.z = -4
CAMERA.offset.y = 1
let customCameraRotation = deg(20)

document.addEventListener('mousedown', e => {
	CAMERA.isMoving = true
	CAMERA.oldX = customCameraRotation + e.y / 500
	CAMERA.oldOffset = CAMERA.offset.z - e.y / 100
})

document.addEventListener('mousemove', e => {
	if (CAMERA.isMoving) {
		customCameraRotation = range(deg(10), CAMERA.oldX - e.y / 500, deg(90))
		CAMERA.offset.z = range(-9, CAMERA.oldOffset + e.y / 100, -2)
	}
})

document.addEventListener('mouseup', e => {
	CAMERA.isMoving = false
})

const KEYS = []

document.addEventListener('keydown', e => {
	KEYS[e.key] = true
})

document.addEventListener('keyup', e => {
	KEYS[e.key] = false
})

const DEBUG = false

const WORLD = []
const CHUNKS = {}
WORLD.time = new Date().getTime()

const sqrt3 = Math.sqrt(3)
const CHUNK_SIDE = 4
const WATER_TRIANGLES_COLOR = 0
const WATER_LINES_COLOR = 1
const WATER_POINTS = [
   -CHUNK_SIDE / 2, 0, 0,                      //0
	0,              0, 0,                      //1
   -CHUNK_SIDE / 4, 0, CHUNK_SIDE * sqrt3 / 4, //2
	CHUNK_SIDE / 2, 0, 0,                      //3
	CHUNK_SIDE / 4, 0, CHUNK_SIDE * sqrt3 / 4, //4
	0,              0, CHUNK_SIDE * sqrt3 / 2, //5
]
const WATER_TRIANGLES = [
	0, 1, 2,
	1, 2, 4,
	1, 3, 4,
	2, 4, 5,
]
const ROCK_TRIANGLES_COLOR = 8
const ROCK_LINES_COLOR = 9
const ROCK_POINTS = [
	0,               0.4, CHUNK_SIDE * sqrt3 / 6,      //0
	0,              -0.2, CHUNK_SIDE * sqrt3 / 12,     //1
   -CHUNK_SIDE / 8, -0.2, CHUNK_SIDE * sqrt3 * 5 / 24, //2
    CHUNK_SIDE / 8, -0.2, CHUNK_SIDE * sqrt3 * 5 / 24, //3
]
const ROCK_TRIANGLES = [
	0, 1, 2,
	0, 1, 3,
	0, 2, 3,
]
const ISLAND_TRIANGLES_COLOR = 2
const ISLAND_LINES_COLOR = 3
const ISLAND_POINTS = [
   -CHUNK_SIDE / 2, -0.2, 0,                       //0
	0,              -0.2, 0,                       //1
   -CHUNK_SIDE / 4,  0.4, CHUNK_SIDE * sqrt3 / 12, //2
   -CHUNK_SIDE / 4, -0.2, CHUNK_SIDE * sqrt3 / 4,  //3
	CHUNK_SIDE / 4,  0.4, CHUNK_SIDE * sqrt3 / 12, //4
	CHUNK_SIDE / 2, -0.2, 0,                       //5
	CHUNK_SIDE / 4, -0.2, CHUNK_SIDE * sqrt3 / 4,  //6
	0,               0.4, CHUNK_SIDE * sqrt3 / 3,  //7
	0,              -0.2, CHUNK_SIDE * sqrt3 / 2,  //8
]
const ISLAND_TRIANGLES = [
	0, 1, 2,
	0, 2, 3,
	1, 2, 4,
	1, 4, 5,
	2, 3, 7,
	2, 4, 7,
	3, 7, 8,
	4, 5, 6,
	4, 6, 7,
	6, 7, 8,
]

const BOAT_BODY = new Mesh(4, 5,
   [0.0, -0.9, -2.6, //0
	0.0,  1.0, -3.4, //1
   -1.6,  1.0,  0.0, //2
	0.0,  0.9,  0.0, //3
	0.0,  1.0,  3.4, //4
	0.0, -0.9,  0.0, //5
	0.0, -0.9,  2.6, //6
	1.6,  1.0,  0.0],//7
   [0, 1, 2,
	0, 2, 5,
	0, 5, 7,
	0, 7, 1,
	1, 3, 2,
	1, 7, 3,
	2, 4, 6,
	3, 4, 2,
	3, 7, 4,
	5, 2, 6,
	5, 6, 7,
	7, 6, 4]
)
BOAT_BODY.translation.z = CHUNK_SIDE * sqrt3 / 3
CAMERA.translation.x = BOAT_BODY.translation.x
CAMERA.translation.y = BOAT_BODY.translation.y
CAMERA.translation.z = BOAT_BODY.translation.z
BOAT_BODY.rotation.x = -Math.PI / 24
BOAT_BODY.rotation.z = Math.PI / 18
BOAT_BODY.scale.x = 0.25
BOAT_BODY.scale.y = 0.25
BOAT_BODY.scale.z = 0.25
WORLD.push(BOAT_BODY)

const BOAT_MAST = new Mesh(4, 5,
   [0.1, 6.0,  0.0, //0
	0.1, 0.9,  0.0, //1
	0.0, 0.9,  0.1, //2
	0.0, 6.0,  0.1, //3
   -0.1, 0.9,  0.0, //4
   -0.1, 6.0,  0.0, //5
	0.0, 0.9, -0.1, //6
	0.0, 6.0, -0.1, //7
	0.1, 1.3,  0.0, //8
	0.1, 1.3,  3.9, //9
	0.0, 1.4,  3.9, //10
	0.0, 1.4,  0.0, //11
   -0.1, 1.3,  3.9, //12
   -0.1, 1.3,  0.0, //13
	0.0, 1.2,  3.9, //14
	0.0, 1.2,  0.0],//15
   [0,  1,  2,
	0,  1,  7,
	0,  2,  3,
	0,  3,  7,
	1,  2,  4,
	1,  4,  6,
	1,  6,  7,
	2,  3,  4,
	3,  4,  5,
	3,  5,  7,
	4,  5,  6,
	5,  6,  7,
	8,  9,  10,
	8,  10, 11,
	8,  9,  15,
	8,  11, 15,
	9,  14, 15,
	9,  10, 12,
	9,  12, 14,
	10, 11, 12,
	11, 13, 15,
	11, 12, 13,
	12, 13, 14,
	13, 14, 15]
)
BOAT_BODY.children.push(BOAT_MAST)
BOAT_MAST.rotation.y = Math.PI

const BOAT_SAIL_POINTS = [
   -0.1, 2.3, 1.5, //0
   	0.0, 1.4, 0.1, //1
   	0.0, 6.0, 0.1, //2
  	0.0, 1.4, 3.9  //3
]
const BOAT_SAIL_TRIANGLES = [
	0, 1, 2,
 	0, 1, 3,
 	0, 2, 3
]
const BOAT_SAIL = new Mesh(6, 7, BOAT_SAIL_POINTS, BOAT_SAIL_TRIANGLES)
BOAT_MAST.children.push(BOAT_SAIL)

const collidablePoints = {}
const BOAT_COLLIDER_Y = 0.6 / 4
const BOAT_COLLIDER_WIDTH = 1.6 / 4  //x
const BOAT_COLLIDER_HEIGHT = 2.6 / 4 //y
const BOAT_COLLIDER_LENGTH = 3.6 / 4 //z
const INTERMEDIATE_COUNT = 3
function rotateRelativeCoord(relativeCoord){
	return [relativeCoord[0], relativeCoord[1], relativeCoord[2], 1].x(rotate(-BOAT_BODY.rotation.y, -BOAT_BODY.rotation.x, -BOAT_BODY.rotation.z, true))
}
function pointRadius(rotatedRelativeCoord){
	return Math.sqrt(Math.pow((rotatedRelativeCoord[0] / BOAT_COLLIDER_WIDTH), 2) + Math.pow(((rotatedRelativeCoord[1] - BOAT_COLLIDER_Y) / BOAT_COLLIDER_HEIGHT), 2) + Math.pow((rotatedRelativeCoord[2] / BOAT_COLLIDER_LENGTH), 2))
}
function pointRadiusPartial(rotatedRelativeCoord, which){
	switch(which){
		case 0: //x
			let currentColliderHeight = BOAT_COLLIDER_HEIGHT * Math.cos(Math.PI / 2 * rotatedRelativeCoord[0] / BOAT_COLLIDER_WIDTH)
			let currentColliderLength = BOAT_COLLIDER_LENGTH * Math.cos(Math.PI / 2 * rotatedRelativeCoord[0] / BOAT_COLLIDER_WIDTH)
			return Math.sqrt(Math.pow((rotatedRelativeCoord[1] - BOAT_COLLIDER_Y) / currentColliderHeight, 2) + Math.pow(rotatedRelativeCoord[2] / currentColliderLength, 2))
		case 1: //y
			let currentColliderWidth = BOAT_COLLIDER_WIDTH * Math.cos(Math.PI / 2 * rotatedRelativeCoord[1] / BOAT_COLLIDER_HEIGHT)
			let currentColliderLength = BOAT_COLLIDER_LENGTH * Math.cos(Math.PI / 2 * rotatedRelativeCoord[1] / BOAT_COLLIDER_HEIGHT)
			return Math.sqrt(Math.pow(rotatedRelativeCoord[0] / currentColliderWidth, 2) + Math.pow(rotatedRelativeCoord[2] / currentColliderLength, 2))
		case 2: //z
			let currentColliderWidth = BOAT_COLLIDER_WIDTH * Math.cos(Math.PI / 2 * rotatedRelativeCoord[2] / BOAT_COLLIDER_LENGTH)
			let currentColliderHeight = BOAT_COLLIDER_HEIGHT * Math.cos(Math.PI / 2 * rotatedRelativeCoord[2] / BOAT_COLLIDER_LENGTH)
			return Math.sqrt(Math.pow(rotatedRelativeCoord[0] / currentColliderWidth, 2) + Math.pow((rotatedRelativeCoord[1] - BOAT_COLLIDER_Y) / currentColliderHeight, 2))
	}
}
function rotatedRelavivePointInside(rotatedRelativeCoord){
	return pointRadius(rotatedRelativeCoord) <= 1
}

const VECTOR1 = new Mesh(7, 7,
   [-BOAT_COLLIDER_WIDTH, 0.0, -BOAT_COLLIDER_LENGTH,
	-BOAT_COLLIDER_WIDTH, 0.0,  BOAT_COLLIDER_LENGTH], [ 0, 1, 0 ])
VECTOR1.offset.y = BOAT_COLLIDER_Y
VECTOR1.translation = BOAT_BODY.translation
VECTOR1.rotation = BOAT_BODY.rotation
const VECTOR2 = new Mesh(7, 7,
   [-BOAT_COLLIDER_WIDTH, -BOAT_COLLIDER_HEIGHT, 0.0,
	-BOAT_COLLIDER_WIDTH,  BOAT_COLLIDER_HEIGHT, 0.0], [ 0, 1, 0 ])
VECTOR2.offset.y = BOAT_COLLIDER_Y
VECTOR2.translation = BOAT_BODY.translation
VECTOR2.rotation = BOAT_BODY.rotation
const VECTOR3 = new Mesh(7, 7,
   [-BOAT_COLLIDER_WIDTH, BOAT_COLLIDER_HEIGHT, 0.0,
	 BOAT_COLLIDER_WIDTH, BOAT_COLLIDER_HEIGHT, 0.0], [ 0, 1, 0 ])
VECTOR3.offset.y = BOAT_COLLIDER_Y
VECTOR3.translation = BOAT_BODY.translation
VECTOR3.rotation = BOAT_BODY.rotation
if (DEBUG){
	WORLD.push(VECTOR1)
	WORLD.push(VECTOR2)
	WORLD.push(VECTOR3)
}

const BOAT_MOTION = new Model()
const CURRENT_WIND = new Model()
const CURRENT_WATER_FLOW = new Model()
const MAX_FLOW_FORCE = 2
CURRENT_WIND.translation.x = 1
CURRENT_WIND.rotation.y = Math.PI / 2
CURRENT_WATER_FLOW.translation.x = 1
CURRENT_WATER_FLOW.rotation.y = 0

const renderCamera = [0, 0]
const renderArea = [].fill(addCircleTile, 18)
const cached = { "-2,1": 1, "-2,2": 1, "-3,2": 1, "-1,2": 1, "-2,3": 1, "-1,3": 1, "0,2": 1, "0,3": 1, "0,-2": 2 }
const water = () => new Mesh(WATER_TRIANGLES_COLOR, WATER_LINES_COLOR, WATER_POINTS, WATER_TRIANGLES)
const island = () => new Mesh(ISLAND_TRIANGLES_COLOR, ISLAND_LINES_COLOR, ISLAND_POINTS, ISLAND_TRIANGLES)
const rock = () => new Mesh(ROCK_TRIANGLES_COLOR, ROCK_LINES_COLOR, ROCK_POINTS, ROCK_TRIANGLES)
function whatIsThere(tile){
	if (cached[tile] != undefined)
		return cached[tile]
	return 0
}
const CENTER_IDS = [2, 4, 7]
const LEFT = 0
const VERTICAL = 1
const RIGHT = 2
function getEdgeID(up, sideNum){
	switch(sideNum){
		case LEFT:
			return up ? 3 : 6
		case RIGHT:
			return up ? 6 : 3
		case VERTICAL:
			return 1
	}
}
function getPointID(up, pointNum){
	switch(pointNum){
		case LEFT:
			return up ? 0 : 5
		case RIGHT:
			return up ? 5 : 0
		case VERTICAL:
			return 8
	}
}
function pointSurroundings(up, pointNum){
	switch(pointNum){
		case LEFT:
			return up ? [ "0,-1", "-1,-1", "-2,-1", "-2,0", "-1,0" ] : [ "-1,0", "-2,0", "-2,1", "-1,1", "0,1" ]
		case RIGHT:
			return up ? [ "1,0",  "2,0", "2,-1", "1,-1", "0,-1" ] : [ "0,1", "1,1", "2,1", "2,0", "1,0" ]
		case VERTICAL:
			return up ? [ "-1,0", "-1,1", "0,1", "1,1", "1,0" ] : [ "1,0", "1,-1", "0,-1", "-1,-1", "-1,0" ]
	}
}
function getPointSurroundingIndices(up, pointNum){
	switch(getPointID(up, pointNum)){
		case 0:
			return [5, 8, 0, 5, 8]
		case 5:
			return [8, 0, 5, 8, 0]
		case 8:
			return [0, 5, 8, 0, 5]
	}
}
function updateIslandEdges(tile){
	let up = tile.up()
	for (let anotherTile of goodSurroundings(tile)){
		let anotherAbsolute = tile.add(anotherTile)
		let edgeID = getEdgeID(up, 1 + anotherTile.x())
		if (cached[anotherAbsolute] == 1)
			if (CHUNKS[anotherAbsolute] && CHUNKS[anotherAbsolute][1]){
				let height = CHUNKS[anotherAbsolute][1].readY(edgeID)
				if (height < 0){
					height = 0.2 + Math.random() / 1.4
					CHUNKS[anotherAbsolute][1].updateY(edgeID, height)
				}
				CHUNKS[tile][1].updateY(edgeID, height)
			} else
				CHUNKS[tile][1].updateY(edgeID, -0.2)
		else
			CHUNKS[tile][1].updateY(edgeID, -0.2)
	}
	for (id of CENTER_IDS)
		CHUNKS[tile][1].updateY(id, 0.2 + Math.random() / 1.4)
}
function updateIslandPoints(tile){
	let up = tile.up()
	for (let point = 0; point < 3; point++){
		let curIndices = getPointSurroundingIndices(up, point)
		let count = 0
		let surr = pointSurroundings(up, point)
		let height = -0.2
		for (let i = 0; i < surr.length; i++){
			let anotherAbsolute = tile.add(surr[i])
			count += cached[anotherAbsolute] == 1
			if (CHUNKS[anotherAbsolute] != undefined && CHUNKS[anotherAbsolute][1] != undefined && CHUNKS[anotherAbsolute][1].cachedPoints.length == 9 * 3)
				height = Math.max(height, CHUNKS[anotherAbsolute][1].readY(curIndices[i]))
		}
		if (height < 0)
			if (count == 5){
				height = 0.2 + Math.random() / 1.4
				for (let i = 0; i < surr.length; i++){
					let anotherAbsolute = tile.add(surr[i])
					if (CHUNKS[anotherAbsolute] != undefined && CHUNKS[anotherAbsolute][1] != undefined)
						CHUNKS[anotherAbsolute][1].updateY(curIndices[i], height)
				}
			}
		CHUNKS[tile][1].updateY(getPointID(up, point), height)
	}
}
function addChunk(chunk, tile){
	chunk.rotation.y = tile.up() ? 0 : Math.PI;
	chunk.translation.z = (tile.up() ? 0 : CHUNK_SIDE * sqrt3 / 2) + CHUNK_SIDE * sqrt3 / 2 * tile.y()
	chunk.translation.x = tile.x() * CHUNK_SIDE / 2
	CHUNKS[tile].push(chunk)
}
const unusedWater = []
const unusedIslands = []
const unusedRocks = []
function shift(delta_x, delta_y){
	renderCamera[0] += delta_x
	renderCamera[1] += delta_y
	for (let tile in collidablePoints)
		if (tile != renderCamera.toString() && surroundings(renderCamera.toString()).indexOf(tile.add(renderCamera.toString().mul(-1))) == -1)
			delete collidablePoints[tile]
	for (let tile in CHUNKS)
		if (renderArea.indexOf(tile.add(renderCamera.toString().mul(-1))) == -1){
			unusedWater.push(CHUNKS[tile][0])
			if (CHUNKS[tile][1])
				if (CHUNKS[tile][1].cachedPoints.length != 4 * 3)
					unusedIslands.push(CHUNKS[tile][1])
				else
					unusedRocks.push(CHUNKS[tile][1])
			delete CHUNKS[tile]
		}
	for (let tile of renderArea){
		let absolute = tile.add(renderCamera)
		if (CHUNKS[absolute] == undefined){
			CHUNKS[absolute] = []
			addChunk(unusedWater.pop() || water(), absolute)
			switch(whatIsThere(absolute)){
				case 1:
					addChunk(unusedIslands.pop() || island(), absolute)
					updateIslandEdges(absolute)
					updateIslandPoints(absolute)
					break
				case 2:
					addChunk(unusedRocks.pop() || rock(), absolute)
			}
		}
		let up = absolute.up()
		if ((tile == "0,0" || surroundings(renderCamera.toString()).indexOf(tile) != -1) && CHUNKS[absolute][1] && collidablePoints[absolute] == undefined){
			collidablePoints[absolute] = []
			for (let i = 0; i < CHUNKS[absolute][1].cachedPoints.length / 3; i++){
				collidablePoints[absolute].push([CHUNKS[absolute][1].cachedPoints[i * 3] * (up ? 1 : -1) + absolute.x() * CHUNK_SIDE / 2,
												 CHUNKS[absolute][1].cachedPoints[i * 3 + 1],
												 CHUNKS[absolute][1].cachedPoints[i * 3 + 2] * (up ? 1 : -1) + (!up) * CHUNK_SIDE * sqrt3 / 2 + absolute.y() * CHUNK_SIDE * sqrt3 / 2])
				if (DEBUG)
					addDebugRock(collidablePoints[absolute][i])
			}
			for (let i = 0; i < CHUNKS[absolute][1].cachedLines.length; i += 2){
				let p1 = CHUNKS[absolute][1].cachedPoints.slice(CHUNKS[absolute][1].cachedLines[i] * 3, CHUNKS[absolute][1].cachedLines[i] * 3 + 3)
				let p2 = CHUNKS[absolute][1].cachedPoints.slice(CHUNKS[absolute][1].cachedLines[i + 1] * 3, CHUNKS[absolute][1].cachedLines[i + 1] * 3 + 3)
				for (let j = 1; j < INTERMEDIATE_COUNT + 1; j++){
					collidablePoints[absolute].push([(p1[0] * j + p2[0] * (INTERMEDIATE_COUNT + 1 - j)) / (INTERMEDIATE_COUNT + 1) * (up ? 1 : -1) + absolute.x() * CHUNK_SIDE / 2,
													 (p1[1] * j + p2[1] * (INTERMEDIATE_COUNT + 1 - j)) / (INTERMEDIATE_COUNT + 1),
													 (p1[2] * j + p2[2] * (INTERMEDIATE_COUNT + 1 - j)) / (INTERMEDIATE_COUNT + 1) * (up ? 1 : -1) + (!up) * CHUNK_SIDE * sqrt3 / 2 + absolute.y() * CHUNK_SIDE * sqrt3 / 2])
					if (DEBUG)
						addDebugRock(collidablePoints[absolute][collidablePoints[absolute].length - 1])
				}
			}
		}
	}
}

function addDebugRock(coord){
	let p = new Mesh(4, 5, ROCK_POINTS, ROCK_TRIANGLES)
	let pScale = 0.3
	p.translation.x = coord[0]
	p.translation.y = coord[1]
	p.translation.z = coord[2] - CHUNK_SIDE * sqrt3 / 6 * pScale
	p.scale.x = pScale
	p.scale.y = pScale
	p.scale.z = pScale
	WORLD.push(p)
}

const SAIL_ANIMATION_LENGTH = 700
const SAIL_ANIMATION_FLOOR = 1.3
let sailState = 1
let removingSail = false

shift(0, 0)

let touch = []
let cachedRotation = 0
space.addEventListener("touchstart", function(touchEvent){
	touch = [touchEvent.touches[0].screenX, touchEvent.touches[0].screenY]
	cachedRotation = BOAT_MAST.rotation.y
	CAMERA.oldX = customCameraRotation
	CAMERA.oldOffset = CAMERA.offset.z - touch[1] / 100
}, false)
space.addEventListener("touchmove", function(touchEvent){
	touchEvent.preventDefault()
	BOAT_MAST.rotation.y = range(Math.PI / 2, cachedRotation - 0.005 * (touchEvent.touches[0].screenX - touch[0]), 3 * Math.PI / 2)
	customCameraRotation = range(deg(10), CAMERA.oldX - (touchEvent.touches[0].screenY - touch[1]) / 500, deg(90))
	CAMERA.offset.z = range(-9, CAMERA.oldOffset + touchEvent.touches[0].screenY / 100, -2)
}, false)

requestAnimationFrame(function render() {
	const newTime = new Date().getTime()
	WORLD.dt = newTime - WORLD.time
	WORLD.time = newTime
	WORLD.timeColor = (0.618 + Math.sign((WORLD.time / 5000) % 4 - 2) * (Math.sqrt(1.25 - Math.pow((WORLD.time / 5000) % 2 - 1, 2)) - 0.5)) / 1.236

	if (WORLD.dt > 10000){
		requestAnimationFrame(render)
		return
	}

	//UPDATES

	let waveState = WORLD.time % 2000 / 2000
	let waveX = (BOAT_BODY.translation.x / 5.0 + waveState) * 2.0 * 3.14
	let waveZ = (BOAT_BODY.translation.z / 5.0 + waveState) * 2.0 * 3.14
	if (Math.sin(waveX) + Math.cos(waveZ) < 0)
		BOAT_MOTION.translation.y = 0.15 * (Math.sin(waveX) + Math.cos(waveZ)) - BOAT_BODY.translation.y
	else
		BOAT_MOTION.translation.y -= 0.001

	let boat = "0,0".add(BOAT_BODY.translation.x, BOAT_BODY.translation.z)
	let delta = renderCamera.toString().toWorldCoordinate().add(boat.mul(-1))
	if (!inside(renderCamera.toString().up(), delta)){
		let nearest = "0,0"
		let nearestToNearest = "0,0"
		while (!inside(renderCamera.toString().add(nearest).up(), delta)){
			for (tile of (delta.len() < CHUNK_SIDE * sqrt3 / 3 ? goodSurroundings(renderCamera.toString().add(nearest)) : surroundings(renderCamera.toString().add(nearest))))
				if (renderCamera.toString().add(nearestToNearest.add(tile)).toWorldCoordinate().add(boat.mul(-1)).len() < renderCamera.toString().add(nearestToNearest).toWorldCoordinate().add(boat.mul(-1)).len())
					nearestToNearest = nearestToNearest.add(tile)
			nearest = nearestToNearest
			delta = renderCamera.toString().add(nearest).toWorldCoordinate().add(boat.mul(-1))
		}
		shift(nearest.x(), nearest.y())
	}

	BOAT_MOTION.rotation.x *= Math.pow(0.99, 1 + 10000 * Math.abs(BOAT_MOTION.rotation.x))
	BOAT_MOTION.rotation.y *= Math.pow(0.99, 1 + 10000 * Math.abs(BOAT_MOTION.rotation.y))
	BOAT_MOTION.rotation.z *= Math.pow(0.99, 1 + 10000 * Math.abs(BOAT_MOTION.rotation.z))
	BOAT_MOTION.translation.x *= Math.pow(0.99, 1 + 10 * Math.abs(BOAT_MOTION.translation.x))
	BOAT_MOTION.translation.z *= Math.pow(0.99, 1 + 10 * Math.abs(BOAT_MOTION.translation.z))

	BOAT_MOTION.rotation.x += -Math.sin(BOAT_BODY.rotation.x) / 150
	BOAT_MOTION.rotation.z += -Math.sin(BOAT_BODY.rotation.z) / 100
	if (KEYS["w"] || KEYS["W"]) {
		BOAT_MOTION.translation.x += 0.00003 * Math.sin(BOAT_BODY.rotation.y)
		BOAT_MOTION.translation.z += 0.00003 * Math.cos(BOAT_BODY.rotation.y)
		BOAT_MOTION.rotation.y *= 0.95
	}
	let speed = Math.sqrt((BOAT_MOTION.translation.x * BOAT_MOTION.translation.x + BOAT_MOTION.translation.z * BOAT_MOTION.translation.z))
	if (KEYS["s"] || KEYS["S"]){
		BOAT_MOTION.translation.x *= 0.99
		BOAT_MOTION.translation.z *= 0.99
		speed *= 0.99
		BOAT_MOTION.rotation.y *= Math.pow(1.001, 1 + speed * 10)
	}
	BOAT_MOTION.translation.x += 0.0005 * Math.sin(BOAT_BODY.rotation.y) * speed
	BOAT_MOTION.translation.z += 0.0005 * Math.cos(BOAT_BODY.rotation.y) * speed

	let apmlitude = CURRENT_WIND.translation.x
	let sailToWater = Math.pow(Math.sqrt(1 / (1 / Math.abs(Math.cos(BOAT_BODY.rotation.x)) + 1 / Math.abs(Math.cos(BOAT_BODY.rotation.z)) - 1)), 7)
	let windToSail = Math.cos(CURRENT_WIND.rotation.y - BOAT_MAST.rotation.y - BOAT_BODY.rotation.y) * sailToWater * sailState
	let windSpeed = Math.sin(BOAT_MAST.rotation.y) * Math.sign(windToSail) * apmlitude
	let sideWindSpeed = 0.1 * Math.cos(BOAT_MAST.rotation.y) * windToSail * apmlitude
	BOAT_MOTION.translation.x += 0.00002 * Math.sin(BOAT_BODY.rotation.y) * windSpeed
	BOAT_MOTION.translation.z += 0.00002 * Math.cos(BOAT_BODY.rotation.y) * windSpeed
	BOAT_MOTION.translation.x -= 0.00002 * Math.cos(BOAT_BODY.rotation.y) * Math.cos(BOAT_SAIL.rotation.y) * sideWindSpeed
	BOAT_MOTION.translation.z += 0.00002 * Math.sin(BOAT_BODY.rotation.y) * Math.cos(BOAT_SAIL.rotation.y) * sideWindSpeed
	BOAT_MOTION.rotation.x += 0.001 * Math.cos(BOAT_BODY.rotation.y) * Math.sin(BOAT_MAST.rotation.y - BOAT_BODY.rotation.y) * windToSail * apmlitude
	BOAT_MOTION.rotation.y -= 0.000003 * Math.abs(Math.sin(BOAT_MAST.rotation.y)) * windToSail * apmlitude
	BOAT_MOTION.rotation.z -= 0.003 * Math.sin(BOAT_BODY.rotation.y) * Math.sin(BOAT_MAST.rotation.y - BOAT_BODY.rotation.y) * windToSail * apmlitude

	let waterAmplitude = CURRENT_WATER_FLOW.translation.x
	let waterToBoat = Math.sin(CURRENT_WATER_FLOW.rotation.y - BOAT_BODY.rotation.y)
	let waterSpeed = waterAmplitude * waterToBoat
	let sideWaterSpeed = 0.3 * Math.cos(CURRENT_WATER_FLOW.rotation.y - BOAT_BODY.rotation.y) * waterAmplitude
	BOAT_MOTION.translation.x += 0.00001 * Math.sin(BOAT_BODY.rotation.y) * waterSpeed
	BOAT_MOTION.translation.z += 0.00001 * Math.cos(BOAT_BODY.rotation.y) * waterSpeed
	BOAT_MOTION.translation.x += 0.00001 * Math.cos(BOAT_BODY.rotation.y) * sideWaterSpeed
	BOAT_MOTION.translation.z += 0.00001 * Math.sin(BOAT_BODY.rotation.y) * sideWaterSpeed
	BOAT_MOTION.rotation.x -= 0.001 * Math.sign(Math.cos(CURRENT_WATER_FLOW.rotation.y - BOAT_BODY.rotation.y)) * waterToBoat * waterAmplitude
	BOAT_MOTION.rotation.y -= 0.000003 * Math.cos(CURRENT_WATER_FLOW.rotation.y - BOAT_BODY.rotation.y) * Math.sign(waterToBoat) * waterAmplitude
	BOAT_MOTION.rotation.z -= 0.003 * sideWaterSpeed * waterAmplitude

	if (KEYS["a"] || KEYS["A"])
		BOAT_MOTION.rotation.y = Math.max(-0.05, BOAT_MOTION.rotation.y - 0.001 * Math.max(speed, 0.02))
	if (KEYS["d"] || KEYS["D"])
		BOAT_MOTION.rotation.y = Math.min(0.05, BOAT_MOTION.rotation.y + 0.001 * Math.max(speed, 0.02))
	if (KEYS["ArrowLeft"])
		BOAT_MAST.rotation.y = Math.min(BOAT_MAST.rotation.y + 0.002 * WORLD.dt, 3 * Math.PI / 2)
	if (KEYS["ArrowRight"])
		BOAT_MAST.rotation.y = Math.max(BOAT_MAST.rotation.y - 0.002 * WORLD.dt, Math.PI / 2)

	for (let tile in collidablePoints)
		for (let point of collidablePoints[tile]){
			let relative = [point[0] - BOAT_BODY.translation.x, point[1] - BOAT_BODY.translation.y, point[2] - BOAT_BODY.translation.z]
			let relativeLength = Math.sqrt(Math.pow(relative[0], 2) + Math.pow(relative[1], 2) + Math.pow(relative[2], 2))
			if (relativeLength <= Math.max(BOAT_COLLIDER_WIDTH, BOAT_COLLIDER_HEIGHT, BOAT_COLLIDER_LENGTH)){
				let rotatedRelativeCoord = rotateRelativeCoord(relative)
				if (rotatedRelavivePointInside(rotatedRelativeCoord)){
					let force = (1 - pointRadius(rotatedRelativeCoord)) * Math.sqrt(BOAT_MOTION.translation.x * BOAT_MOTION.translation.x + BOAT_MOTION.translation.z * BOAT_MOTION.translation.z)
					BOAT_MOTION.translation.x -= relative[0] / relativeLength * force * 10
					BOAT_MOTION.translation.y -= relative[1] / relativeLength * force * 10
					BOAT_MOTION.translation.z -= relative[2] / relativeLength * force * 10
					BOAT_MOTION.rotation.x += Math.sign(rotatedRelativeCoord[1]) * Math.sign(rotatedRelativeCoord[2]) * Math.sqrt((1 - rotatedRelativeCoord[2] / Math.sqrt(rotatedRelativeCoord[1] * rotatedRelativeCoord[1] + rotatedRelativeCoord[2] * rotatedRelativeCoord[2])) / 2) * force * 50000
					BOAT_MOTION.rotation.y -= Math.sign(rotatedRelativeCoord[0]) * Math.sign(rotatedRelativeCoord[2]) * Math.sqrt((1 - rotatedRelativeCoord[2] / Math.sqrt(rotatedRelativeCoord[0] * rotatedRelativeCoord[0] + rotatedRelativeCoord[2] * rotatedRelativeCoord[2])) / 2) * force * 100
					BOAT_MOTION.rotation.z -= Math.sign(rotatedRelativeCoord[0]) * Math.sign(rotatedRelativeCoord[1]) * Math.sqrt((1 - rotatedRelativeCoord[1] / Math.sqrt(rotatedRelativeCoord[0] * rotatedRelativeCoord[0] + rotatedRelativeCoord[1] * rotatedRelativeCoord[1])) / 2) * force * 10000
				}
			}
		}

	BOAT_BODY.translation.x += BOAT_MOTION.translation.x * WORLD.dt
	BOAT_BODY.translation.y += BOAT_MOTION.translation.y * Math.min(WORLD.dt / 700, 1)
	BOAT_BODY.translation.z += BOAT_MOTION.translation.z * WORLD.dt
	BOAT_BODY.rotation.x += BOAT_MOTION.rotation.x * 10 * Math.min(WORLD.dt / 1000, 1)
	BOAT_BODY.rotation.y += BOAT_MOTION.rotation.y * WORLD.dt
	BOAT_BODY.rotation.z += BOAT_MOTION.rotation.z * 10 * Math.min(WORLD.dt / 1000, 1)
	CAMERA.translation.x += (BOAT_BODY.translation.x - CAMERA.translation.x) * Math.min(WORLD.dt / 400, 1)
	CAMERA.translation.y += (BOAT_BODY.translation.y - CAMERA.translation.y) * Math.min(WORLD.dt / 400, 1)
	CAMERA.translation.z += (BOAT_BODY.translation.z - CAMERA.translation.z) * Math.min(WORLD.dt / 400, 1)
	CAMERA.rotation.x = customCameraRotation + (BOAT_BODY.rotation.x - CAMERA.rotation.x + customCameraRotation) * Math.min(WORLD.dt / 1000, 1)
	CAMERA.rotation.y += (BOAT_BODY.rotation.y - CAMERA.rotation.y) * Math.min(WORLD.dt / 1000, 1)
	CAMERA.rotation.z += (BOAT_BODY.rotation.z - CAMERA.rotation.z) * Math.min(WORLD.dt / 1000, 1)

	if (KEYS["ArrowUp"] && !KEYS["ArrowDown"])
		removingSail = false
	if (KEYS["ArrowDown"] && !KEYS["ArrowUp"])
		removingSail = true
	if (removingSail)
		sailState = Math.max(sailState - WORLD.dt / SAIL_ANIMATION_LENGTH, 0)
	else
		sailState = Math.min(sailState + WORLD.dt / SAIL_ANIMATION_LENGTH, 1)
	BOAT_SAIL.updateX(0, -1.3 * CURRENT_WIND.translation.x / MAX_FLOW_FORCE * windToSail * sailState)
	for (let i = 1; i < BOAT_SAIL_POINTS.length; i += 3)
		BOAT_SAIL.updateY(Math.floor(i / 3), SAIL_ANIMATION_FLOOR + (BOAT_SAIL_POINTS[i] - SAIL_ANIMATION_FLOOR) * sailState)

	//UPDATES done

	const empty = mat()
	const projection = perspective(deg(90), space.clientWidth / space.clientHeight, 0.1, 100)

	space.width = space.clientWidth
	space.height = space.clientHeight
	gl.viewport(0, 0, space.clientWidth, space.clientHeight)
	gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
	gl.clearColor(WORLD.timeColor, WORLD.timeColor, WORLD.timeColor, 1)

	gl.useProgram(MESH_PROGRAM)
	gl.uniformMatrix4fv(MESH_PROGRAM_PROPERTIES.uProjection, false, projection)
	gl.uniformMatrix4fv(MESH_PROGRAM_PROPERTIES.uView, false, CAMERA.inverse())
	gl.uniform3f(MESH_PROGRAM_PROPERTIES.uFogPivot,
			BOAT_BODY.translation.x, BOAT_BODY.translation.y, BOAT_BODY.translation.z)

	for (let chunks in CHUNKS)
		for (let i = 1; i < CHUNKS[chunks].length; i++)
			CHUNKS[chunks][i].render(MESH_PROGRAM_PROPERTIES, empty)
	WORLD.forEach(it => it.render(MESH_PROGRAM_PROPERTIES, empty))

	gl.useProgram(WATER_PROGRAM)
	gl.uniformMatrix4fv(WATER_PROGRAM_PROPERTIES.uProjection, false, projection)
	gl.uniformMatrix4fv(WATER_PROGRAM_PROPERTIES.uView, false, CAMERA.inverse())
	gl.uniform1f(WATER_PROGRAM_PROPERTIES.uState, WORLD.time % 2000 / 2000)
	gl.uniform3f(WATER_PROGRAM_PROPERTIES.uFogPivot,
			BOAT_BODY.translation.x, BOAT_BODY.translation.y, BOAT_BODY.translation.z)

	for (let chunks in CHUNKS)
		CHUNKS[chunks][0].render(WATER_PROGRAM_PROPERTIES, empty)

	requestAnimationFrame(render)
})
