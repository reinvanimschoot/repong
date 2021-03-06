# RePong

A [ReasonML](https://reasonml.github.io/) implementation of Pong, built with [Reprocessing](https://github.com/Schmavery/reprocessing)

![alt text](./assets/screenshot.png "RePong")

### Install

```
yarn
```

### Build

```
yarn build
```

### Start

```
yarn start
```

To build to JS run `npm run build:web` and then run a static server, like `python -m SimpleHTTPServer` and go to `localhost:8000`. If you're using safari you can simply open the `index.html` and tick `Develop > Disable Cross-Origin Restrictions`.

To build to native run `npm run build:native` and run `npm run start:native`

The build system used is [bsb-native](https://github.com/bsansouci/bsb-native).
