open Reprocessing;

type winner =
  | Computer
  | Player;

type playing =
  | Menu
  | Playing
  | Restart(winner);

type state = {
  playing,
  computerCoords: (float, float),
  playerCoords: (float, float),
  ballCoords: (float, float),
  ballVelocity: (float, float),
  font: fontT,
  playerScore: int,
  computerScore: int,
};

/* Measurements */
let windowWidth = 600.;
let windowHeight = 400.;

let paddleHeight = 100.;
let paddleWidth = 10.;
let paddleVelocity = 250.;

let ballRadius = 10.;

/* Colors */
let background = Utils.color(~r=40, ~g=42, ~b=54, ~a=255);
let foreground = Utils.color(~r=80, ~g=250, ~b=123, ~a=255);

/* Helpers */
let paddleCenter = (~paddleY) => paddleY +. paddleHeight /. 2.;

let constrainPaddle = (~amt) => Utils.constrain(~amt, ~low=0., ~high=windowHeight -. paddleHeight);

let playerScoredPoint = ({ballCoords}) => {
  let (ballX, _) = ballCoords;
  ballX < -. ballRadius;
};

let computerScoredPoint = ({ballCoords}) => {
  let (ballX, _) = ballCoords;
  ballX > +. windowWidth +. ballRadius;
};

module PlayerPaddle = {
  /* Default Settings */
  let defaultCoords = (windowWidth -. paddleWidth, windowHeight /. 2.);

  /* Functions */
  let draw = (state, env) => {
    Draw.fill(foreground, env);
    Draw.rectf(~pos=state.playerCoords, ~width=paddleWidth, ~height=paddleHeight, env);
  };

  let update = ({playerCoords}, env) => {
    let deltaTime = Env.deltaTime(env);

    let (playerPaddleX, playerPaddleY) = playerCoords;

    if (Env.key(Up, env)) {
      (playerPaddleX, constrainPaddle(~amt=playerPaddleY -. paddleVelocity *. deltaTime));
    } else if (Env.key(Down, env)) {
      (playerPaddleX, constrainPaddle(~amt=playerPaddleY +. paddleVelocity *. deltaTime));
    } else {
      playerCoords;
    };
  };
};

module ComputerPaddle = {
  /* Default Settings */
  let defaultCoords = (0., windowHeight /. 2.);
  let deadzoneTresshold = 30.;

  /* Helpers */
  let underDeadzone = (~ballY, ~paddleY) => ballY < paddleCenter(~paddleY) -. deadzoneTresshold;
  let aboveDeadzone = (~ballY, ~paddleY) => ballY > paddleCenter(~paddleY) +. deadzoneTresshold;

  /* Functions */
  let draw = (state, env) => {
    Draw.fill(foreground, env);
    Draw.rectf(~pos=state.computerCoords, ~width=paddleWidth, ~height=paddleHeight, env);
  };

  let update = ({computerCoords, ballCoords}, env) => {
    let deltaTime = Env.deltaTime(env);

    let (_, ballY) = ballCoords;
    let (computerPaddleX, computerPaddleY) = computerCoords;

    if (underDeadzone(~ballY, ~paddleY=computerPaddleY)) {
      (computerPaddleX, constrainPaddle(~amt=computerPaddleY -. paddleVelocity *. deltaTime));
    } else if (aboveDeadzone(~ballY, ~paddleY=computerPaddleY)) {
      (computerPaddleX, constrainPaddle(~amt=computerPaddleY +. paddleVelocity *. deltaTime));
    } else {
      computerCoords;
    };
  };
};

module Ball = {
  /* Default setting */
  let acceleration = 10.;
  let defaultVelocity = (Utils.randomf(~min=-250., ~max=250.), Utils.randomf(~min=-250., ~max=250.));
  let defaultCoords = (windowWidth /. 2., windowHeight /. 2.);

  /* Helpers */
  let collidesWithWalls = (~ballY) => ballY < 0. +. ballRadius || ballY > windowHeight -. ballRadius;

  let collisionVelocity = (ballY, paddleY) => {
    let paddleCenter = paddleY +. paddleHeight /. 2.;
    let ballCenter = ballY +. ballRadius /. 2.;

    let distanceToMiddle = abs_float(ballCenter -. paddleCenter);

    ballCenter <= paddleCenter ? -. distanceToMiddle *. acceleration : distanceToMiddle *. acceleration;
  };

  let collidesWithPaddle = (~ballCoords, ~paddleCoords) =>
    Utils.intersectRectCircle(
      ~rectPos=paddleCoords,
      ~rectH=paddleHeight,
      ~rectW=paddleWidth,
      ~circlePos=ballCoords,
      ~circleRad=ballRadius,
    );

  let calculateVelocity = ({ballCoords, playerCoords, computerCoords, ballVelocity}, _env) => {
    let (_, ballY) = ballCoords;
    let (_, playerPaddleY) = playerCoords;
    let (_, computerPaddleY) = computerCoords;
    let (velocityX, velocityY) = ballVelocity;

    if (collidesWithPaddle(~ballCoords, ~paddleCoords=playerCoords)) {
      (velocityX *. (-1.), collisionVelocity(ballY, playerPaddleY));
    } else if (collidesWithPaddle(~ballCoords, ~paddleCoords=computerCoords)) {
      (velocityX *. (-1.), collisionVelocity(ballY, computerPaddleY));
    } else if (collidesWithWalls(~ballY)) {
      (velocityX, velocityY *. (-1.));
    } else {
      (velocityX, velocityY);
    };
  };

  /* Functions */
  let draw = ({ballCoords}, env) => {
    Draw.fill(foreground, env);
    Draw.ellipsef(~center=ballCoords, ~radx=ballRadius, ~rady=ballRadius, env);
  };

  let update = (state, env) => {
    let deltaTime = Env.deltaTime(env);
    let (ballX, ballY) = state.ballCoords;

    let (velocityX, velocityY) = calculateVelocity(state, env);

    ((ballX +. velocityX *. deltaTime, ballY +. velocityY *. deltaTime), (velocityX, velocityY));
  };
};

module Arena = {
  let draw = ({font, playerScore, computerScore}, env) => {
    Draw.background(background, env);

    Draw.text(~font, ~body=string_of_int(computerScore), ~pos=(200, 50), env);
    Draw.text(~font, ~body=string_of_int(playerScore), ~pos=(400, 50), env);
  };
};

let setup = env => {
  Env.size(~width=int_of_float(windowWidth), ~height=int_of_float(windowHeight), env);

  {
    playing: Menu,
    computerCoords: ComputerPaddle.defaultCoords,
    playerCoords: PlayerPaddle.defaultCoords,
    ballCoords: Ball.defaultCoords,
    ballVelocity: Ball.defaultVelocity,
    font: Draw.loadFont(~filename="assets/fonts/font.fnt", ~isPixel=true, env),
    playerScore: 0,
    computerScore: 0,
  };
};

let draw = (state, env) => {
  Arena.draw(state, env);

  Ball.draw(state, env);

  PlayerPaddle.draw(state, env);
  ComputerPaddle.draw(state, env);

  switch (state.playing) {
  | Menu =>
    Draw.text(~font=state.font, ~body="Press Space to Play", ~pos=(175, 300), env);

    Env.key(Space, env) ? {...state, playing: Playing} : state;
  | Playing =>
    let playerCoords = PlayerPaddle.update(state, env);
    let computerCoords = ComputerPaddle.update(state, env);

    if (playerScoredPoint(state)) {
      let ballCoords = Ball.defaultCoords;
      let ballVelocity = Ball.defaultVelocity;
      let playerScore = state.playerScore + 1;
      let playing = playerScore == 3 ? Restart(Player) : Playing;

      {...state, playing, playerScore, computerCoords, playerCoords, ballCoords, ballVelocity};
    } else if (computerScoredPoint(state)) {
      let ballCoords = Ball.defaultCoords;
      let ballVelocity = Ball.defaultVelocity;
      let computerScore = state.computerScore + 1;
      let playing = computerScore == 3 ? Restart(Computer) : Playing;

      {...state, playing, computerScore, computerCoords, playerCoords, ballCoords, ballVelocity};
    } else {
      let (ballCoords, ballVelocity) = Ball.update(state, env);
      {...state, computerCoords, playerCoords, ballCoords, ballVelocity};
    };

  | Restart(winner) =>
    let text =
      switch (winner) {
      | Player => "You won!"
      | Computer => "You Lost!"
      };

    Draw.text(~font=state.font, ~body=text, ~pos=(240, 300), env);
    Draw.text(~font=state.font, ~body="Press Space to play again", ~pos=(140, 340), env);

    if (Env.key(Space, env)) {
      {...state, playing: Playing, computerScore: 0, playerScore: 0};
    } else {
      state;
    };
  };
};

run(~setup, ~draw, ());