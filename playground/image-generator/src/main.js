const doc = `Generates some images.

Usage:
  ./generate all [-c NUM] [-o OUTPUT] [FOLDERS...]

Options:
  -c, --count NUM      Generate only NUM images total [Default: -1]
  -o, --output OUTPUT  Where to save generated images [Default: out]
`;

import { docopt } from "docopt"
import cartesian from "cartesian-product-generator"
import fs from "fs"
import path from "path"
import sharp from "sharp"

const args = docopt(doc, { version: "1.0.0" })

const all = (folders, maxCount, outputDirpath) => {
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

  const combinations = cartesian.product(...Object.values(files))
  for (let count = 0; maxCount < 0 || count < maxCount; count++) {
    const combination = combinations.next()

    if (combination.done) {
      break
    }

    const filepaths = combination.value

    const filenames = filepaths.map(f => path.basename(f))
    console.log(`Generating ${filenames}`)

    const outputFilepath = path.join(outputDirpath, `${count}.png`)
    const images = filepaths.map(f => ({ input: f}))
    const bottom = images.shift()

    sharp(bottom.input).
      composite(images).
      toFile(outputFilepath, err => {
        if (err) {
          console.error(`Failed to generate ${filenames} - ${err}`)
        }
      })
  }

  console.log(`Done!`)
}

if (args["all"]) {
  const folders = args["FOLDERS"]
  const maxCount = parseInt(args["--count"])
  const outputDirpath = args["--output"]

  all(folders, maxCount, outputDirpath)
}
