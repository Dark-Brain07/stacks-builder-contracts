# Stacks Builder Contracts

A collection of Clarity smart contracts for the Stacks blockchain. Built by **rajuice** for the Stacks Builder Rewards campaign.

## 📦 Contracts

This repository contains 4 production-ready Clarity smart contracts:

### 1. SIP-010 Token (`sip010-token.clar`)
A fungible token implementation following the SIP-010 standard.
- **Token Name**: Stacks Builder Token (SBT)
- **Decimals**: 6
- **Features**: Transfer, Mint (owner), Burn, Token URI

### 2. SIP-009 NFT (`sip009-nft.clar`)
A non-fungible token implementation following the SIP-009 standard.
- **Collection Name**: Stacks Builder NFT
- **Max Supply**: 10,000
- **Features**: 
  - Public minting with STX payment
  - Owner minting (for airdrops)
  - Marketplace functionality (list, buy, unlist)
  - Toggle minting, adjust price

### 3. Staking Pool (`staking-pool.clar`)
A staking contract for earning rewards on staked STX.
- **Minimum Stake**: 1 STX
- **Lock Period**: ~24 hours (144 blocks)
- **Reward Rate**: 1% per period
- **Features**:
  - Stake/Unstake STX
  - Claim rewards
  - Compound rewards
  - Emergency withdraw (owner)

### 4. DAO Voting (`dao-voting.clar`)
A decentralized governance contract for proposals and voting.
- **Voting Period**: ~7 days (1008 blocks)
- **Minimum Votes**: 3 to pass
- **Features**:
  - Create proposals
  - Vote on proposals
  - Execute passed proposals
  - Join/Leave DAO with STX stake
  - Member management

## 🚀 Quick Start

### Prerequisites
- [Clarinet](https://github.com/stx-labs/clarinet) - Clarity development tool
- [Leather Wallet](https://leather.io) - Stacks wallet for deployment

### Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/stacks-builder-contracts.git
cd stacks-builder-contracts

# Install dependencies
npm install

# Check contracts
clarinet check
```

### Testing

```bash
# Run tests
npm test

# Interactive console
clarinet console
```

### Deployment

#### Deploy to Testnet
```bash
# Get testnet STX from: https://explorer.stacks.co/sandbox/faucet?chain=testnet

# Generate deployment plan
clarinet deployments generate --testnet

# Deploy
clarinet deployments apply --testnet
```

#### Deploy to Mainnet
```bash
# Make sure you have STX in your wallet

# Generate deployment plan
clarinet deployments generate --mainnet

# Deploy
clarinet deployments apply --mainnet
```

## 📁 Project Structure

```
stacks-builder-contracts/
├── Clarinet.toml          # Project configuration
├── contracts/
│   ├── sip010-token.clar  # Fungible token
│   ├── sip009-nft.clar    # Non-fungible token
│   ├── staking-pool.clar  # Staking contract
│   └── dao-voting.clar    # DAO governance
├── settings/
│   └── Devnet.toml        # Devnet configuration
├── tests/                 # Unit tests
└── package.json
```

## 🔧 Technologies Used

- **Clarity** - Smart contract language for Stacks
- **Clarinet** - Development and testing framework
- **@stacks/connect** - Wallet connection
- **@stacks/transactions** - Transaction building

## 📜 License

MIT License

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## 🔗 Links

- [Stacks Documentation](https://docs.stacks.co)
- [Clarity Language Reference](https://docs.stacks.co/clarity/language-overview)
- [Clarinet Documentation](https://docs.hiro.so/clarinet)
- [Talent Protocol](https://talent.app)

---

Built with ❤️ for the Stacks ecosystem
