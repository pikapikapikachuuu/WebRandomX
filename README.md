# WebRandomX

WebRandomX is a JavaScript/WASM implementation of [RandomX](https://github.com/tevador/RandomX) PoW for web use cases.

## Build

### Linux

#### Get Source code

```
git clone https://github.com/pikapikapikachuuu/WebRandomX.git
```

#### WASM

Prerequisites: emcc, cmake, make
```
cd WebRandomX/src
mkdir bin && cd bin
emcmake cmake -DARCH=native ..
make
```

#### Web App

Prerequisites: npm
```
cd WebRandomX
npm install
```

For development, run:

```
npm run dev
```

 Then lauch Chrome debugger through VSCode or manually.
