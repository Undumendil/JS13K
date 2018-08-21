const gl = space.getContext("webgl")
if (!gl) throw new Error("Could not initialize WebGL")

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

class Mesh extends Model {
	constructor(trianglesColor, linesColor, vertices) {
		super()
		this.children = []

		const lines = calculateLines(vertices)
		this.linesColor = linesColor
		this.linesLength = lines.length / 3
		this.trianglesColor = trianglesColor
		this.trianglesLength = vertices.length / 3
		this.trianglesBuffer = createArrayBuffer(vertices)
		this.linesBuffer = createArrayBuffer(lines)
	}

	render(properties, parentModel) {
		const model = this.total().x(parentModel)
		this.children.forEach(it => it.render(properties, model))
		gl.uniformMatrix4fv(properties.uModel, false, model)

		properties.aPosition.set(this.trianglesBuffer, 3)
		gl.uniform1f(properties.uTimeColor, WORLD.timeColor)
		gl.uniform1f(properties.uForward, 0.0)
		gl.uniform1i(properties.uColor, this.trianglesColor)
		gl.drawArrays(gl.TRIANGLES, 0, this.trianglesLength)

		properties.aPosition.set(this.linesBuffer, 3)
		gl.uniform1f(properties.uTimeColor, WORLD.timeColor)
		gl.uniform1f(properties.uForward, 0.001)
		gl.uniform1i(properties.uColor, this.linesColor)
		gl.drawArrays(gl.LINES, 0, this.linesLength)
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

function createArrayBuffer(data) {
	const buffer = gl.createBuffer()
	gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
	gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(data), gl.STATIC_DRAW);
	gl.bindBuffer(gl.ARRAY_BUFFER, null)
	return buffer
}

function modifyArrayBuffer(buffer, anchors, data) {
	const data32 = new Float32Array(data)
	gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
	for (let i = 0; i < anchors.length; i++) {
		gl.bufferSubData(gl.ARRAY_BUFFER, anchors[i] * 3 * 4, data32)
	}
	gl.bindBuffer(gl.ARRAY_BUFFER, null)
	return buffer
}

function extract(data, index, size) {
	const out = []
	for (let i = 0; i < size; i++)
		out.push(data[index + i])
	return out
}

function append(target, data, size) {
	for (let i = 0; i < size; i++)
		target.push(data[i])
}

function cache(container, target, a, b) {
	const pa = a.join(',')
	const pb = b.join(',')
	const ab = pa + ':' + pb
	const ba = pb + ':' + pa
	if (!container[ab] && !container[ba]) {
		container[ab] = container[ba] = true
		append(target, a, 3)
		append(target, b, 3)
	}
}

function calculateLines(vertices) {
	const found = {}
	const lines = []
	for (let i = 0; i < vertices.length; i += 9) {
		const a = extract(vertices, i, 3)
		const b = extract(vertices, i + 3, 3)
		const c = extract(vertices, i + 6, 3)
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
		if (uColor == 2) //GRASS_TRIANGLES_COLOR
			return vec3(0.3 - 0.1 * uTimeColor, 0.8 - 0.2 * uTimeColor, 0.3 - 0.1 * uTimeColor);
		if (uColor == 3) //GRASS_LINES_COLOR
			return vec3(0.2 - 0.1 * uTimeColor, 0.5 - 0.2 * uTimeColor, 0.2 - 0.1 * uTimeColor);
		if (uColor == 4 || uColor == 6) //BOAT_BODY_TRIANGLES_COLOR; BOAT_MAST_TRIANGLES_COLOR
			return vec3(0.6 - 0.3 * uTimeColor, 0.5 - 0.2 * uTimeColor, 0.4 - 0.2 * uTimeColor);
		if (uColor == 5 || uColor == 7) //BOAT_BODY_LINES_COLOR; BOAT_MAST_LINES_COLOR
			return vec3(0.5 - 0.3 * uTimeColor, 0.4 - 0.2 * uTimeColor, 0.3 - 0.1 * uTimeColor);
		if (uColor == 8) //BOAT_SAIL_TRIANGLES_COLOR
			return vec3(1.0 - 0.3 * uTimeColor, 1.0 - 0.3 * uTimeColor, 1.0 - 0.3 * uTimeColor);
		if (uColor == 9) //BOAT_SAIL_LINES_COLOR
			return vec3(0.6 - 0.2 * uTimeColor, 0.6 - 0.2 * uTimeColor, 0.6 - 0.2 * uTimeColor);
		if (uColor == 10) //ROCK_TRIANGLES_COLOR
			return vec3(0.6 - 0.4 * uTimeColor, 0.6 - 0.4 * uTimeColor, 0.6 - 0.4 * uTimeColor);
		if (uColor == 11) //ROCK_LINES_COLOR
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

document.addEventListener('mousedown', e => {
	CAMERA.isMoving = true
	CAMERA.oldX = CAMERA.rotation.x + e.y / 500
	CAMERA.oldY = CAMERA.rotation.y + e.x / 500
})

document.addEventListener('mousemove', e => {
	if (CAMERA.isMoving) {
		CAMERA.rotation.x = range(deg(10), CAMERA.oldX - e.y / 500, deg(90))
	}
})

document.addEventListener('mouseup', e => {
	CAMERA.isMoving = false
})

document.addEventListener('wheel', e => {
	CAMERA.offset.z = range(-10, CAMERA.offset.z - e.deltaY / 100, -2)
})

const KEYS = []

document.addEventListener('keydown', e => {
	KEYS[e.key] = true
})

document.addEventListener('keyup', e => {
	KEYS[e.key] = false
})

const WORLD = []
const WATER = []
WORLD.time = new Date().getTime()

const sqrt3 = Math.sqrt(3)
const CHUNK_SIDE = 4
const WATER_TRIANGLES_COLOR = 0
const WATER_LINES_COLOR = 1
const WATER_VERTICES = [
	-CHUNK_SIDE / 2, 0, 0,
	 0,              0, 0,
	-CHUNK_SIDE / 4, 0, CHUNK_SIDE * sqrt3 / 4,
	 0,              0, 0,
	 CHUNK_SIDE / 2, 0, 0,
	 CHUNK_SIDE / 4, 0, CHUNK_SIDE * sqrt3 / 4,
	-CHUNK_SIDE / 4, 0, CHUNK_SIDE * sqrt3 / 4,
	 CHUNK_SIDE / 4, 0, CHUNK_SIDE * sqrt3 / 4,
	 0,              0, CHUNK_SIDE * sqrt3 / 2,
	 0,              0, 0,
	 CHUNK_SIDE / 4, 0, CHUNK_SIDE * sqrt3 / 4,
	-CHUNK_SIDE / 4, 0, CHUNK_SIDE * sqrt3 / 4,
]
const ROCK_TRIANGLES_COLOR = 10
const ROCK_LINES_COLOR = 11
const ROCK_VERTICES = [
	 0,               0.4, CHUNK_SIDE * sqrt3 / 6,
	 0,              -0.2, CHUNK_SIDE * sqrt3 / 12,
	-CHUNK_SIDE / 8, -0.2, CHUNK_SIDE * sqrt3 * 5 / 24,
	 0,               0.4, CHUNK_SIDE * sqrt3 / 6,
	 0,              -0.2, CHUNK_SIDE * sqrt3 / 12,
	 CHUNK_SIDE / 8, -0.2, CHUNK_SIDE * sqrt3 * 5 / 24,
	 0,               0.4, CHUNK_SIDE * sqrt3 / 6,
	 CHUNK_SIDE / 8, -0.2, CHUNK_SIDE * sqrt3 * 5 / 24,
	-CHUNK_SIDE / 8, -0.2, CHUNK_SIDE * sqrt3 * 5 / 24,
]
const GRASS_TRIANGLES_COLOR = 2
const GRASS_LINES_COLOR = 3
const CHUNK_VERTICES = [
	-CHUNK_SIDE / 2, -0.2, 0,
	 0,              -0.2, 0,
	-CHUNK_SIDE / 4,  0.4, CHUNK_SIDE * sqrt3 / 12,
	-CHUNK_SIDE / 2, -0.2, 0,
	-CHUNK_SIDE / 4,  0.4, CHUNK_SIDE * sqrt3 / 12,
	-CHUNK_SIDE / 4, -0.2, CHUNK_SIDE * sqrt3 / 4,
	-CHUNK_SIDE / 4,  0.4, CHUNK_SIDE * sqrt3 / 12,
	 0,              -0.2, 0,
	 CHUNK_SIDE / 4,  0.4, CHUNK_SIDE * sqrt3 / 12,
	 0,              -0.2, 0,
	 CHUNK_SIDE / 2, -0.2, 0,
	 CHUNK_SIDE / 4,  0.4, CHUNK_SIDE * sqrt3 / 12,
	 CHUNK_SIDE / 4,  0.4, CHUNK_SIDE * sqrt3 / 12,
	 CHUNK_SIDE / 2, -0.2, 0,
	 CHUNK_SIDE / 4, -0.2, CHUNK_SIDE * sqrt3 / 4,
	 CHUNK_SIDE / 4,  0.4, CHUNK_SIDE * sqrt3 / 12,
	 CHUNK_SIDE / 4, -0.2, CHUNK_SIDE * sqrt3 / 4,
	 0,               0.4, CHUNK_SIDE * sqrt3 / 3,
	 0,               0.4, CHUNK_SIDE * sqrt3 / 3,
	 CHUNK_SIDE / 4, -0.2, CHUNK_SIDE * sqrt3 / 4,
	 0,              -0.2, CHUNK_SIDE * sqrt3 / 2,
	 0,              -0.2, CHUNK_SIDE * sqrt3 / 2,
	-CHUNK_SIDE / 4, -0.2, CHUNK_SIDE * sqrt3 / 4,
	 0,               0.4, CHUNK_SIDE * sqrt3 / 3,
	-CHUNK_SIDE / 4, -0.2, CHUNK_SIDE * sqrt3 / 4,
	-CHUNK_SIDE / 4,  0.4, CHUNK_SIDE * sqrt3 / 12,
	 0,               0.4, CHUNK_SIDE * sqrt3 / 3,
	-CHUNK_SIDE / 4,  0.4, CHUNK_SIDE * sqrt3 / 12,
	 CHUNK_SIDE / 4,  0.4, CHUNK_SIDE * sqrt3 / 12,
	 0,               0.4, CHUNK_SIDE * sqrt3 / 3,
]

const BOAT_BODY = new Mesh(4, 5,
   [0.0, -0.9, -2.6,
	0.0,  1.0, -3.4,
   -1.6,  1.0,  0.0,
	0.0,  0.9,  0.0,
	0.0,  1.0,  3.4,
   -1.6,  1.0,  0.0,
	0.0,  1.0, -3.4,
	0.0,  0.9,  0.0,
   -1.6,  1.0,  0.0,
	0.0, -0.9, -2.6,
   -1.6,  1.0,  0.0,
	0.0, -0.9,  0.0,
   -1.6,  1.0,  0.0,
	0.0,  1.0,  3.4,
	0.0, -0.9,  2.6,
	0.0, -0.9,  0.0,
   -1.6,  1.0,  0.0,
	0.0, -0.9,  2.6,
	0.0, -0.9, -2.6,
	1.6,  1.0,  0.0,
	0.0,  1.0, -3.4,
	0.0,  0.9,  0.0,
	1.6,  1.0,  0.0,
	0.0,  1.0,  3.4,
	0.0,  1.0, -3.4,
	1.6,  1.0,  0.0,
	0.0,  0.9,  0.0,
	0.0, -0.9, -2.6,
	0.0, -0.9,  0.0,
	1.6,  1.0,  0.0,
	1.6,  1.0,  0.0,
	0.0, -0.9,  2.6,
	0.0,  1.0,  3.4,
	0.0, -0.9,  0.0,
	0.0, -0.9,  2.6,
	1.6,  1.0,  0.0,]
)
CAMERA.translation.x = BOAT_BODY.translation.x
CAMERA.translation.y = BOAT_BODY.translation.y
CAMERA.translation.z = BOAT_BODY.translation.z
BOAT_BODY.rotation.x = -Math.PI / 24
BOAT_BODY.rotation.z = Math.PI / 18
BOAT_BODY.scale.x = 0.25
BOAT_BODY.scale.y = 0.25
BOAT_BODY.scale.z = 0.25
WORLD.push(BOAT_BODY)

const BOAT_MAST = new Mesh(6, 7,
   [0.1, 6.0, 0.0,
	0.1, 0.9, 0.0,
	0.0, 0.9, 0.1,
	0.0, 6.0, 0.1,
	0.0, 0.9, 0.1,
   -0.1, 0.9, 0.0,
   -0.1, 6.0, 0.0,
   -0.1, 0.9, 0.0,
	0.0, 0.9, -0.1,
	0.0, 6.0, -0.1,
	0.0, 0.9, -0.1,
	0.1, 0.9, 0.0,
   -0.1, 0.9, 0.0,
	0.0, 0.9, 0.1,
	0.1, 0.9, 0.0,
	0.0, 6.0, 0.1,
   -0.1, 6.0, 0.0,
	0.0, 6.0, -0.1,
	0.1, 6.0, 0.0,
	0.0, 0.9, 0.1,
	0.0, 6.0, 0.1,
	0.0, 6.0, 0.1,
   -0.1, 0.9, 0.0,
   -0.1, 6.0, 0.0,
   -0.1, 6.0, 0.0,
	0.0, 0.9, -0.1,
	0.0, 6.0, -0.1,
	0.0, 6.0, -0.1,
	0.1, 0.9, 0.0,
	0.1, 6.0, 0.0,
   -0.1, 0.9, 0.0,
	0.1, 0.9, 0.0,
	0.0, 0.9, -0.1,
	0.0, 6.0, 0.1,
	0.0, 6.0, -0.1,
	0.1, 6.0, 0.0,
	0.1, 1.3, 0.0,
	0.1, 1.3, 3.9,
	0.0, 1.4, 3.9,
	0.0, 1.4, 0.0,
	0.0, 1.4, 3.9,
   -0.1, 1.3, 3.9,
   -0.1, 1.3, 0.0,
   -0.1, 1.3, 3.9,
	0.0, 1.2, 3.9,
	0.0, 1.2, 0.0,
	0.0, 1.2, 3.9,
	0.1, 1.3, 3.9,
   -0.1, 1.3, 3.9,
	0.0, 1.4, 3.9,
	0.1, 1.3, 3.9,
	0.0, 1.4, 0.0,
   -0.1, 1.3, 0.0,
	0.0, 1.2, 0.0,
	0.1, 1.3, 0.0,
	0.0, 1.4, 3.9,
	0.0, 1.4, 0.0,
	0.0, 1.4, 0.0,
   -0.1, 1.3, 3.9,
   -0.1, 1.3, 0.0,
   -0.1, 1.3, 0.0,
	0.0, 1.2, 3.9,
	0.0, 1.2, 0.0,
	0.0, 1.2, 0.0,
	0.1, 1.3, 3.9,
	0.1, 1.3, 0.0,
   -0.1, 1.3, 3.9,
	0.1, 1.3, 3.9,
	0.0, 1.2, 3.9,
	0.0, 1.4, 0.0,
	0.0, 1.2, 0.0,
	0.1, 1.3, 0.0,]
)
BOAT_BODY.children.push(BOAT_MAST)
BOAT_MAST.rotation.y = Math.PI

const BOAT_SAIL = new Mesh(8, 9,
  [-0.1, 2.3, 1.5,
	0.0, 1.4, 0.1,
	0.0, 6.0, 0.1,
	0.0, 1.4, 0.1,
   -0.1, 2.3, 1.5,
	0.0, 1.4, 3.9,
   -0.1, 2.3, 1.5,
	0.0, 6.0, 0.1,
	0.0, 1.4, 3.9,]
)
BOAT_MAST.children.push(BOAT_SAIL)
const BOAT_SAIL_POINTS = [0, 4, 6]

const BOAT_MOTION = new Model()
const CURRENT_WIND = new Model()
const MAX_FLOW_FORCE = 2
CURRENT_WIND.translation.x = 2
CURRENT_WIND.rotation.y = Math.PI / 2

let flip = false
for (let k = -10; k < 11; k++) {
	for (let i = -10; i < 11; i++) {
		const WATER_CHUNK = new Mesh(WATER_TRIANGLES_COLOR, WATER_LINES_COLOR, WATER_VERTICES)
		WATER_CHUNK.rotation.y = flip ? 0 : Math.PI;
		WATER_CHUNK.translation.z = (flip ? 0 : CHUNK_SIDE * sqrt3 / 2) + CHUNK_SIDE * sqrt3 / 2 * k
		WATER_CHUNK.translation.x = i * CHUNK_SIDE / 2
		WATER.push(WATER_CHUNK)
		flip = !flip
	}
}

const GRASS_CHUNK = new Mesh(GRASS_TRIANGLES_COLOR, GRASS_LINES_COLOR, CHUNK_VERTICES)
GRASS_CHUNK.translation.x -= 4
WORLD.push(GRASS_CHUNK)

const ROCK_CHUNK = new Mesh(ROCK_TRIANGLES_COLOR, ROCK_LINES_COLOR, ROCK_VERTICES)
WORLD.push(ROCK_CHUNK)

requestAnimationFrame(function render() {
	const newTime = new Date().getTime()
	WORLD.dt = newTime - WORLD.time
	WORLD.time = newTime
	WORLD.timeColor = (0.618 + Math.sign((WORLD.time / 5000) % 4 - 2) * (Math.sqrt(1.25 - Math.pow((WORLD.time / 5000) % 2 - 1, 2)) - 0.5)) / 1.236

	//UPDATES

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
		BOAT_MOTION.rotation.y *= Math.pow(1.001, speed * 10)
	}
	BOAT_MOTION.translation.x += 0.0005 * Math.sin(BOAT_BODY.rotation.y) * speed
	BOAT_MOTION.translation.z += 0.0005 * Math.cos(BOAT_BODY.rotation.y) * speed

	let apmlitude = CURRENT_WIND.translation.x
	let sailToWater = Math.pow(Math.sqrt(1 / (1 / Math.cos(BOAT_BODY.rotation.x) + 1 / Math.cos(BOAT_BODY.rotation.z) - 1)), 7)
	let windToSail = Math.cos(CURRENT_WIND.rotation.y - BOAT_MAST.rotation.y - BOAT_BODY.rotation.y) * sailToWater
	let windSpeed = Math.sin(BOAT_MAST.rotation.y) * Math.sign(windToSail) * apmlitude
	let sideWindSpeed = 0.1 * Math.cos(BOAT_MAST.rotation.y) * windToSail * apmlitude
	BOAT_MOTION.translation.x += 0.00002 * Math.sin(BOAT_BODY.rotation.y) * windSpeed
	BOAT_MOTION.translation.z += 0.00002 * Math.cos(BOAT_BODY.rotation.y) * windSpeed
	BOAT_MOTION.translation.x -= 0.00002 * Math.cos(BOAT_BODY.rotation.y) * Math.cos(BOAT_SAIL.rotation.y) * sideWindSpeed
	BOAT_MOTION.translation.z += 0.00002 * Math.sin(BOAT_BODY.rotation.y) * Math.cos(BOAT_SAIL.rotation.y) * sideWindSpeed
	BOAT_MOTION.rotation.x += 0.002 * Math.cos(BOAT_BODY.rotation.y) * Math.sin(BOAT_MAST.rotation.y - BOAT_BODY.rotation.y) * windToSail * apmlitude
	BOAT_MOTION.rotation.y -= 0.000003 * Math.abs(Math.sin(BOAT_MAST.rotation.y)) * windToSail * apmlitude
	BOAT_MOTION.rotation.z -= 0.003 * Math.sin(BOAT_BODY.rotation.y) * Math.sin(BOAT_MAST.rotation.y - BOAT_BODY.rotation.y) * windToSail * apmlitude

	if (KEYS["a"] || KEYS["A"])
		BOAT_MOTION.rotation.y = Math.max(-0.05, BOAT_MOTION.rotation.y - 0.001 * Math.max(speed, 0.02))
	if (KEYS["d"] || KEYS["D"])
		BOAT_MOTION.rotation.y = Math.min(0.05, BOAT_MOTION.rotation.y + 0.001 * Math.max(speed, 0.02))
	if (KEYS["ArrowLeft"])
		BOAT_MAST.rotation.y = Math.min(BOAT_MAST.rotation.y + 0.002 * WORLD.dt, 3 * Math.PI / 2)
	if (KEYS["ArrowRight"])
		BOAT_MAST.rotation.y = Math.max(BOAT_MAST.rotation.y - 0.002 * WORLD.dt, Math.PI / 2)

	BOAT_BODY.translation.x += BOAT_MOTION.translation.x * WORLD.dt
	BOAT_BODY.translation.z += BOAT_MOTION.translation.z * WORLD.dt
	BOAT_BODY.rotation.x += BOAT_MOTION.rotation.x * 10 * Math.min(WORLD.dt / 1000, 1)
	BOAT_BODY.rotation.y += BOAT_MOTION.rotation.y * WORLD.dt
	BOAT_BODY.rotation.z += BOAT_MOTION.rotation.z * 10 * Math.min(WORLD.dt / 1000, 1)
	CAMERA.translation.x += (BOAT_BODY.translation.x - CAMERA.translation.x) * Math.min(WORLD.dt / 400, 1)
	CAMERA.translation.y += (BOAT_BODY.translation.y - CAMERA.translation.y) * Math.min(WORLD.dt / 400, 1)
	CAMERA.translation.z += (BOAT_BODY.translation.z - CAMERA.translation.z) * Math.min(WORLD.dt / 400, 1)
	CAMERA.rotation.x = 0.01 + CAMERA.rotation.x + (BOAT_BODY.rotation.x - CAMERA.rotation.x) * Math.min(WORLD.dt / 1000, 1)
	CAMERA.rotation.y += (BOAT_BODY.rotation.y - CAMERA.rotation.y) * Math.min(WORLD.dt / 1000, 1)
	CAMERA.rotation.z += (BOAT_BODY.rotation.z - CAMERA.rotation.z) * Math.min(WORLD.dt / 1000, 1)

	const sail = 1.3 * CURRENT_WIND.translation.x / MAX_FLOW_FORCE * Math.cos(CURRENT_WIND.rotation.y - BOAT_MAST.rotation.y - BOAT_BODY.rotation.y + Math.PI)
	modifyArrayBuffer(BOAT_SAIL.trianglesBuffer, BOAT_SAIL_POINTS, [sail, 2.3, 1.5])
	modifyArrayBuffer(BOAT_SAIL.linesBuffer, BOAT_SAIL_POINTS, [sail, 2.3, 1.5])

	//UPDATES done

	const empty = mat()
	const projection = perspective(deg(90), space.clientWidth / space.clientHeight, 0.1, 100)
	// const projection = scale(0.3, 0.3, 1).x(ortho(space.clientWidth / space.clientHeight, 100))

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

	WORLD.forEach(it => it.render(MESH_PROGRAM_PROPERTIES, empty))

	gl.useProgram(WATER_PROGRAM)
	gl.uniformMatrix4fv(WATER_PROGRAM_PROPERTIES.uProjection, false, projection)
	gl.uniformMatrix4fv(WATER_PROGRAM_PROPERTIES.uView, false, CAMERA.inverse())
	gl.uniform1f(WATER_PROGRAM_PROPERTIES.uState, WORLD.time % 2000 / 2000)
	gl.uniform3f(WATER_PROGRAM_PROPERTIES.uFogPivot,
			BOAT_BODY.translation.x, BOAT_BODY.translation.y, BOAT_BODY.translation.z)

	WATER.forEach(it => it.render(WATER_PROGRAM_PROPERTIES, empty))

	requestAnimationFrame(render)
})