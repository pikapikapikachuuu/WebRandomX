import Runner from './runner'

;(async () => {
  const runner = await new Runner('engine')
  console.log(runner.module)
})()
