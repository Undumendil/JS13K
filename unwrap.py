import sys

fileName = sys.argv[1]

name = None
verticesIn = []
verticesOut = []


def displayCurrent():
	print(name + ' [')

	for it in range(len(verticesOut) // 3):
		print('\t' + str(verticesOut[it * 3]) + ', ' + str(verticesOut[it * 3 + 1]) + ', ' + str(verticesOut[it * 3 + 2]) + ', ')

	print(']\n')


with open(fileName, 'r') as file:
	line = file.readline()

	while line:
		words = line.split(' ')

		if words[0] == 'o':
			if len(verticesOut) > 0:
				displayCurrent()
				verticesOut = []
			name = words[1].replace('\n', '')

		elif words[0] == 'v':
			verticesIn.append(round(float(words[1]) * 10) / 10)
			verticesIn.append(round(float(words[2]) * 10) / 10)
			verticesIn.append(round(float(words[3]) * 10) / 10)

		elif words[0] == 'f':
			vert = (int(words[1]) - 1) * 3
			verticesOut.append(verticesIn[vert])
			verticesOut.append(verticesIn[vert + 1])
			verticesOut.append(verticesIn[vert + 2])

			vert = (int(words[3]) - 1) * 3
			verticesOut.append(verticesIn[vert])
			verticesOut.append(verticesIn[vert + 1])
			verticesOut.append(verticesIn[vert + 2])

			vert = (int(words[2]) - 1) * 3
			verticesOut.append(verticesIn[vert])
			verticesOut.append(verticesIn[vert + 1])
			verticesOut.append(verticesIn[vert + 2])

		line = file.readline()


if len(verticesOut) > 0:
	displayCurrent()
