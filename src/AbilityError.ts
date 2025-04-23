export class AbilityError extends Error {
  constructor(message: string) {
    super(message);
  }
}

export class AbilityParserError extends AbilityError {
  constructor(message: string) {
    super(message);
  }
}
