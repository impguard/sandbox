const doc = `Generates some images.

Usage:
  ./generate all [-c NUM] [-o OUTPUT] [FOLDERS...]
  ./generate random [-c NUM] [-o OUTPUT] [FOLDERS...]

Options:
  -c, --count NUM      Generate only NUM images total [Default: -1]
  -o, --output OUTPUT  Where to save generated images [Default: out]
`;

const { docopt } = require("docopt")
const cartesian = require("cartesian-product-generator")
const fs = require("fs")
const path = require("path")
const sharp = require("sharp")

const args = docopt(doc, { version: "1.0.0" })

const composite = filepaths => {
  const images = filepaths.map(f => ({ input: f}))
  const bottom = images.shift()
  return sharp(bottom.input).composite(images)
}

const generateAll = (folders, files, maxCount) => {
  const combinations = cartesian.product(folders.map(f => files[f]))
  for (let count = 0; maxCount < 0 || count < maxCount; count++) {
    const combination = combinations.next()

    if (combination.done) {
      break
    }

    const filepaths = combination.value
    const filenames = filepaths.map(f => path.basename(f))

    const outputFilepath = path.join(outputDirpath, `${count}.png`)
    composite(filepaths).toFile(outputFilepath, err => {
        if (err) {
          console.error(`Failed to generate ${filenames} - ${err}`)
        } else {
          console.log(`Generated ${filenames}`)
        }
      })
  }
}

const generateRandom = (folders, files, maxCount) => {
  for (let count = 0; maxCount < 0 || count < maxCount; count++) {
    const filepaths = folders.map(f => files[f]).map(f => {
      const index = Math.floor(Math.random() * f.length)
      return f[index]
    })
    const filenames = filepaths.map(f => path.basename(f))

    const outputFilepath = path.join(outputDirpath, `${count}.png`)
    composite(filepaths).toFile(outputFilepath, err => {
        if (err) {
          console.error(`Failed to generate ${filenames} - ${err}`)
        } else {
          console.log(`Generated ${filenames}`)
        }
      })
  }
}

const generate = (folders, maxCount, outputDirpath, random=false) => {
  const fakeFolders = folders.filter(folder => !fs.existsSync(folder))

  if (fakeFolders.length > 0) {
    console.error(`Folders "${fakeFolders}" do not exist!`)
    process.exit(1)
  }

  const files = folders.reduce((mapping, folder) => {
    const files = fs.readdirSync(folder).
      map(f => path.join(folder, f)).
      filter(f => fs.statSync(f).isFile()).
      sort()
    mapping[folder] = files
    return mapping
  }, {})

  if (!fs.existsSync(outputDirpath)) {
    fs.mkdirSync(outputDirpath, {"recursive": true})
  }

  const countString = maxCount > 0 ? maxCount : "all"
  console.log(`Generating ${countString} images...`)

  if (!random) {
    generateAll(folders, files, maxCount)
  } else {
    generateRandom(folders, files, maxCount)
  }

  console.log(`Done!`)
}

const folders = args["FOLDERS"]
const maxCount = parseInt(args["--count"])
const outputDirpath = args["--output"]

if (args["all"]) {
  generate(folders, maxCount, outputDirpath, false)
} else if (args["random"]) {
  generate(folders, maxCount, outputDirpath, true)
}
