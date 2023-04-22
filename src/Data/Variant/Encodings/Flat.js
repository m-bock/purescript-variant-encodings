export const unsafeGet = (key) => (obj) => obj[key];

export const unsafeDelete =
  (key) =>
  ({ [key]: value, ...rest }) =>
    rest;

export const unsafeInsert = (key) => (val) => (obj) => ({
    ...obj,
    [key]: val
})