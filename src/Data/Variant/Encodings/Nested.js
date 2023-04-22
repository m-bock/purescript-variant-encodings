export const unsafeGet = (key) => (obj) => obj[key];

export const unsafeInsert = (key) => (val) => (obj) => ({
    ...obj,
    [key]: val
})