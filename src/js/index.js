import Runner from './runner.js'

const runApi = async () => {
  const runner = await new Runner('randomx-api-example2')
  runner.module._main()
}

window.onload = runApi
