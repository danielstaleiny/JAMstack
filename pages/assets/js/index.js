import('./dynamic').then((js) => js.run())

import pure from './pure/Main'
import tool from './pure/Tool'

console.log('Hello world from /js/index.js!')

pure.main()
tool.run()
