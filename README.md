### Getting started

Requirements: Node 8+, Yarn.

Clone the repo, navigate to the project in a terminal and run `yarn`.

To run both the client and server locally in development mode, run `yarn serve`.

Alternatively, the server can be run independently by first running `yarn build` and then `node server.js`. The client can then be served locally using Parcel if you wish.

Note: When serving locally, the client will probably need a single refresh before use. It's probably related to the hot-reloading, but it's not happy the first time.

### Implementation

Server-side:

- Node
- socket.io

Client-side:

- React
- AntDesign
- socket.io-client