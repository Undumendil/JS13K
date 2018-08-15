const gl = space.getContext("webgl")
if (!gl) throw new Error("Could not initialie WebGL")

gl.clearColor(0, 0, 0, 1)
gl.clearDepth(1)
gl.enable(gl.DEPTH_TEST)
gl.depthFunc(gl.LEQUAL)
gl.disable(gl.BLEND)

class StaticMesh {
	constructor(color, indicesCount, vertexBuffer, normalsBuffer) {
		this.model = new Model()
		this.normalsBuffer = normalsBuffer
		this.indicesCount = indicesCount
		this.vertexBuffer = vertexBuffer
		this.color = color
	}

	render(program, properties) {
		properties.aNormal.set(this.normalsBuffer, 3)
		properties.aPosition.set(this.vertexBuffer, 3)
		gl.uniform3f(properties.uColor, this.color[0], this.color[1], this.color[2])
		gl.uniformMatrix4fv(properties.uModel, false, this.model.total())
		gl.drawArrays(gl.TRIANGLES, 0, this.indicesCount)
	}
}

class Attribute {
	constructor(program, name) {
		this.attrib = gl.getAttribLocation(program, name)
	}

	set(buffer, size) {
		gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
		gl.vertexAttribPointer(this.attrib, size, gl.FLOAT, false, 0, 0)
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

class Model {
	constructor() {
		this.translation = { x: 0, y: 0, z: 0 }
		this.rotation = { y: 0, x: 0, z: 0 }
		this.offset = { x: 0, y: 0, z: 0 }
		this.scale = { x: 1, y: 1, z: 1 }
	}

	inverse() {
		return translate(-this.translation.x, -this.translation.y, -this.translation.z)
				.MxM(rotate(-this.rotation.y, -this.rotation.x, -this.rotation.z))
				.MxM(translate(-this.offset.x, -this.offset.y, -this.offset.z))
				.MxM(scale(1 / this.scale.x, 1 / this.scale.y, 1 / this.scale.z))
	}

	total() {
		return scale(this.scale.x, this.scale.y, this.scale.z)
				.MxM(translate(this.offset.x, this.offset.y, this.offset.z))
				.MxM(rotate(this.rotation.y, this.rotation.x, this.rotation.z, true))
				.MxM(translate(this.translation.x, this.translation.y, this.translation.z))
	}
}

Array.prototype.MxM = function(m) {
	const out = new Array(16)
	for (let i = 0; i < 4; i++)
		for (let j = 0; j < 4; j++) {
			out[j * 4 + i] = 0
			for (let k = 0; k < 4; k++)
				out[j * 4 + i] += m[i + k * 4] * this[j * 4 + k]
		}
	return out
}

Array.prototype.VxM = function(m) {
	return [
		m[0] * this[0] + m[4] * this[1] +  m[8] * this[2] + m[12] * this[3],
		m[1] * this[0] + m[5] * this[1] +  m[9] * this[2] + m[13] * this[3],
		m[2] * this[0] + m[6] * this[1] + m[10] * this[2] + m[14] * this[3],
		m[3] * this[0] + m[7] * this[1] + m[11] * this[2] + m[15] * this[3]
	]
}

Array.prototype.VxV = function(v) {
	return [
		this[1] * v[2] - this[2] * v[1],
		this[2] * v[0] - this[0] * v[2],
		this[0] * v[1] - this[1] * v[0],
		0
	]
}

Array.prototype.VminusV = function(v) {
	return [ v[0] - this[0], v[1] - this[1], v[2] - this[2] ]
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
	return inverse ? rotZ.MxM(rotX).MxM(rotY) : rotY.MxM(rotX).MxM(rotZ)
}

function compile(type, source) {
	let shader = gl.createShader(type)
	gl.shaderSource(shader, source)
	gl.compileShader(shader)

	if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
		console.error("ShaderProgram: could not compile " +
			(type == gl.VERTEX_SHADER ? "vertex" : "fragment") + "shader")
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

function createNormalsBuffer(vertices) {
	const normals = []
	for (let i = 0; i < vertices.length; i += 9) {
		const a = extract(vertices, i, 3)
		const b = extract(vertices, i + 3, 3)
		const c = extract(vertices, i + 6, 3)
		const ab = b.VminusV(a)
		const ac = c.VminusV(a)
		const n = ab.VxV(ac)
		append(normals, n, 3)
		append(normals, n, 3)
		append(normals, n, 3)
	}
	return createArrayBuffer(normals)
}

function range(lower, target, higher) {
	if (target < lower) return lower
	if (target > higher) return higher
	return target
}

const VERTEX_SHADER = compile(gl.VERTEX_SHADER, `
	precision lowp float;
	attribute vec3 aPosition;
	attribute vec3 aNormal;
	uniform mat4 uModel;
	uniform mat4 uView;
	uniform mat4 uProjection;
	varying vec3 vNormal;

	void main(void) {
		gl_Position = uProjection * uView * uModel * vec4(aPosition, 1.0);
		vNormal = aNormal;
	}
`)

const MESH_FRAGMENT_SHADER = compile(gl.FRAGMENT_SHADER, `
	precision lowp float;
	uniform vec3 uColor;
	varying vec3 vNormal;

	void main(void) {
		gl_FragColor = vec4(uColor, 1.0);
	}
`)

const MESH_PROGRAM = link(VERTEX_SHADER, MESH_FRAGMENT_SHADER)
const MESH_PROGRAM_PROPERTIES = {}
MESH_PROGRAM_PROPERTIES.aPosition = new Attribute(MESH_PROGRAM, "aPosition").enable()
MESH_PROGRAM_PROPERTIES.aNormal = new Attribute(MESH_PROGRAM, "aNormal").enable()
MESH_PROGRAM_PROPERTIES.uColor = gl.getUniformLocation(MESH_PROGRAM, "uColor")
MESH_PROGRAM_PROPERTIES.uModel = gl.getUniformLocation(MESH_PROGRAM, "uModel")
MESH_PROGRAM_PROPERTIES.uView = gl.getUniformLocation(MESH_PROGRAM, "uView")
MESH_PROGRAM_PROPERTIES.uProjection = gl.getUniformLocation(MESH_PROGRAM, "uProjection")

const CAMERA = new Model()

document.addEventListener('mousedown', e => {
	CAMERA.isMoving = true
	CAMERA.oldX = CAMERA.rotation.x
	CAMERA.oldY = CAMERA.rotation.y
	CAMERA.pivotX = e.x
	CAMERA.pivotY = e.y
})

document.addEventListener('mousemove', e => {
	if (CAMERA.isMoving) {
		CAMERA.rotation.y = CAMERA.oldY - (e.x - CAMERA.pivotX) / 500
		CAMERA.rotation.x = range(deg(10), CAMERA.oldX - (e.y - CAMERA.pivotY) / 500, deg(90))
	}
})

document.addEventListener('mouseup', e => {
	CAMERA.isMoving = false
})

const verts = [
	-0.5, -0.5, 0,
	 0.5,  0.5, 0,
	-0.5,  0.5, 0,
	-0.5, -0.5, 0,
	 0.5, -0.5, 0,
	 0.5,  0.5, 0,
]

const vertexBuffer = createArrayBuffer(verts)
const normalsBuffer = createNormalsBuffer(verts)
const rect = new StaticMesh([0.3, 0.3, 1], 6, vertexBuffer, normalsBuffer)
rect.model.rotation.x = deg(90)
// rect.model.translation.z = 3

CAMERA.rotation.x = deg(10)
CAMERA.translation.y = 0.5

requestAnimationFrame(function render() {
	space.width = space.clientWidth
	space.height = space.clientHeight
	gl.viewport(0, 0, space.clientWidth, space.clientHeight)
	gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
	gl.useProgram(MESH_PROGRAM)
	const projection = perspective(deg(90), space.clientWidth / space.clientHeight, 0.1, 100)
	// const projection = ortho(space.clientWidth / space.clientHeight, 100)
	gl.uniformMatrix4fv(MESH_PROGRAM_PROPERTIES.uProjection, false, projection)
	gl.uniformMatrix4fv(MESH_PROGRAM_PROPERTIES.uView, false, CAMERA.inverse())
	rect.render(MESH_PROGRAM, MESH_PROGRAM_PROPERTIES)
	requestAnimationFrame(render)
})
