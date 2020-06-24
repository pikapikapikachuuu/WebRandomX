import WASMDriver from './wasm-driver'
import { RANDOMX_HASH_SIZE } from './constants'

;(async () => {
  const driver = await new WASMDriver('randomx-api-example1')

  const raw = driver.example()

  const bias = raw >> 0
  let str = ''
  for (let i = 0; i < RANDOMX_HASH_SIZE; i++) {
    str += (driver.heapChar[bias + i] & 0xff).toString(16)
  }
  console.log(str)
  driver.freeBuff(raw)
})()
