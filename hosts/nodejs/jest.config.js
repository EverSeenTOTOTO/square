module.exports = {
  clearMocks: true,
  coverageDirectory: 'coverage',
  coverageProvider: 'v8',
  moduleNameMapper: {
    '^@/(.*)$': '<rootDir>/src/$1',
  },
  rootDir: '.',
  testEnvironment: 'node',
  testMatch: [
    '**/*.test.ts',
  ],
  testPathIgnorePatterns: [
    '/node_modules/',
  ],
  transformIgnorePatterns: [
    '/node_modules/',
    '\\.pnp\\.[^\\/]+$',
  ],
};
