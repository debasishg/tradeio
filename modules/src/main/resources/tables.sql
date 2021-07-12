CREATE TYPE accountType AS ENUM ('trading', 'settlement', 'both');

CREATE TABLE IF NOT EXISTS accounts (
    no varchar NOT NULL PRIMARY KEY,
    name varchar NOT NULL,
    type accountType NOT NULL,
    dateOfOpen timestamp NOT NULL,
    dateOfClose timestamp,
    baseCurrency varchar NOT NULL,
    tradingCurrency varchar,
    settlementCurrency varchar
);

CREATE TYPE instrumentType AS ENUM ('ccy', 'equity', 'fixed_income');

CREATE TABLE IF NOT EXISTS instruments (
    isinCode varchar NOT NULL PRIMARY KEY,
    name varchar NOT NULL,
    type instrumentType NOT NULL,
    dateOfIssue timestamp,
    dateOfMaturity timestamp,
    lotSize smallint,
    unitPrice decimal,
    couponRate decimal,
    couponFrequency decimal
);

CREATE TYPE buySell AS ENUM ('buy', 'sell');

CREATE TABLE IF NOT EXISTS orders (
    no varchar NOT NULL PRIMARY KEY,
    dateOfOrder timestamp NOT NULL,
    accountNo varchar references accounts(no)
);

CREATE TABLE IF NOT EXISTS lineItems (
    lineItemId serial PRIMARY KEY,
    orderNo varchar references orders(no),
    isinCode varchar references instruments(isinCode),
    quantity decimal NOT NULL,
    unitPrice decimal NOT NULL,
    buySellFlag buySell NOT NULL
);

CREATE TABLE IF NOT EXISTS executions (
    executionRefNo varchar NOT NULL PRIMARY KEY,
    accountNo varchar NOT NULL references accounts(no),
    orderNo varchar NOT NULL references orders(no),
    isinCode varchar NOT NULL references instruments(isinCode),
    market varchar NOT NULL,
    buySellFlag buySell NOT NULL,
    unitPrice decimal NOT NULL,
    quantity decimal NOT NULL,
    dateOfExecution timestamp NOT NULL
);

CREATE TABLE IF NOT EXISTS trades (
    tradeRefNo varchar NOT NULL PRIMARY KEY,
    accountNo varchar NOT NULL references accounts(no),
    isinCode varchar NOT NULL references instruments(isinCode),
    market varchar NOT NULL,
    buySellFlag buySell NOT NULL,
    unitPrice decimal NOT NULL,
    quantity decimal NOT NULL,
    tradeDate timestamp NOT NULL,
    valueDate timestamp,
    netAmount decimal
);

CREATE TABLE IF NOT EXISTS tradeTaxFees (
    tradeTaxFeeId serial PRIMARY KEY,
    tradeRefNo varchar NOT NULL references trades(tradeRefNo),
    taxFeeId varchar NOT NULL references taxFees(taxFeeId),
    amount decimal NOT NULL
);

CREATE TABLE IF NOT EXISTS taxFees (
    taxFeeId varchar NOT NULL PRIMARY KEY,
    description varchar NOT NULL
);

CREATE TABLE IF NOT EXISTS balance (
    balanceId serial PRIMARY KEY,
    accountNo varchar NOT NULL UNIQUE references accounts(no),
    amount decimal NOT NULL,
    asOf timestamp NOT NULL,
    currency varchar NOT NULL
);