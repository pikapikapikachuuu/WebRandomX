# WebRandomX

WebRandomX is a JavaScript/WASM implementation of [RandomX](https://github.com/tevador/RandomX) PoW for web use cases.

## Build

### Linux
Prerequisites: emcc, cmake, make
```
git clone https://github.com/pikapikapikachuuu/WebRandomX.git
cd WebRandomX
mkdir build && cd build
emcmake cmake -DARCH=native .. (optional) [-DWASM_DEBUG=xxx]
make
```

