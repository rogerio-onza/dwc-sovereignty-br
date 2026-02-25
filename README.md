# 🇧🇷 DarwinCore  - Repatriação de Estudos Brasileiros

[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![Darwin Core](https://img.shields.io/badge/Darwin_Core-Standard-75B375?style=for-the-badge&logoColor=black)](#)
[![Licença](https://img.shields.io/badge/License-MIT-yellow?style=for-the-badge)](https://opensource.org/licenses/MIT)
[![Claude](https://img.shields.io/badge/AI_Assisted-Claude-D97757?style=for-the-badge&logo=anthropic&logoColor=white)](#)

**Ferramentas e fluxos de trabalho em R para padronização, validação e garantia da soberania de dados de biodiversidade.**

## 📖 Sobre o Projeto

Este repositório centraliza scripts e rotinas em R voltados para a **repatriação de dados biológicos**. O objetivo central é promover a **soberania de dados** da biodiversidade brasileira, fornecendo ferramentas para que pesquisadores estruturem e convertam suas planilhas para o padrão Darwin Core (DwC). 

Ao facilitar esse processo de padronização e validação taxonômica rigorosa, o projeto visa incentivar a publicação de dados de alta qualidade em repositórios e infraestruturas nacionais, mitigando as implicações e a fragmentação causadas quando pesquisadores publicam dados primários exclusivamente em plataformas estrangeiras.

## ✨ Destaques Técnicos e Funcionalidades

O fluxo de trabalho foi desenhado para lidar com as complexidades reais dos dados de coleções e levantamentos em campo, aplicando regras estritas de formatação DwC:

- 🔍 **Validação Taxonômica Local e Global**: Conferência de nomes científicos priorizando bases brasileiras (florabr, faunabr), com fallback para bases globais (taxadb/GBIF).
- 🛡️ **Integração de Status de Ameaça**: Cruzamento automático com a Lista de Espécies Ameaçadas do Brasil (MMA Portaria 148/2022) e dados locais/IUCN.
- ⚙️ **Mapeamento Rigoroso DwC**: Transformação de dados customizados com lógica avançada de extração e formatação, incluindo:
  - Uso estrito do separador " | " no campo recordedBy para múltiplos coletores.
  - Validação de correspondência exata no início do texto (ao invés de busca parcial) para a definição do basisOfRecord.
  - Preenchimento inteligente do campo locality, restrito a áreas protegidas/unidades de conservação.
  - Lógica customizada para extração precisa do institutionCode a partir de PDFs ou documentos de referência.
- 📋 **Auditoria Completa**: Geração automática de relatórios (_auditoria.xlsx) registrando cada decisão taxonômica, sinônimos resolvidos e marcadores de incerteza (ex: cf., aff.).

## 📂 Estrutura do Repositório

```text
dwc-sovereignty-br/
├── 📜 README.md
├── 📁 R/                            # Scripts principais do fluxo de trabalho
│   └── 📄 neotropical_carnivores_dwc.R  # 1
    └── 📄 jaguar_movement_database_dwc.R  # 2

```

## 🚀 Como Começar

### Pré-requisitos

Certifique-se de ter o R instalado e os seguintes pacotes essenciais:

```r
install.packages(c("readxl", "dplyr", "stringr", "tidyr", "purrr", "writexl"))
# Pacotes taxonômicos (verifique a documentação de cada um para instalação):
# - florabr
# - faunabr
# - taxadb
```

### Uso Básico

1. Clone este repositório:
   ```bash
   git clone [https://github.com/rogerio-onza/dwc-sovereignty-br.git](https://github.com/rogerio-onza/dwc-sovereignty-br.git)
   ```
2. Adicione sua planilha de dados brutos na pasta data/.
3. Abra o script principal na pasta R/, ajuste os caminhos dos arquivos se necessário.
4. Execute o pipeline para gerar o dataset padronizado e a planilha de auditoria na pasta outputs/.

## 🔄 Artigos repatriados

1. [NEOTROPICAL CARNIVORES: a data set on carnivore distribution in the Neotropics](https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecy.3128)
2. [Jaguar movement database: a GPS-based movement dataset of an apex predator in the Neotropics](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecy.2379)

## 🤝 Contribuindo

Contribuições que fortaleçam as ferramentas de soberania e padronização de dados são muito bem-vindas! 

1. Faça um Fork do projeto
2. Crie uma Branch para sua modificação (git checkout -b feature/NovaValidacao)
3. Faça o Commit de suas mudanças (git commit -m 'Adiciona suporte a novo campo DwC')
4. Faça o Push para a branch (git push origin feature/NovaValidacao)
5. Abra um Pull Request

## 📜 Licença

MIT

## ✍️ Autor e Contato

**Rogerio Nunes Oliveira**

Pesquisa, Modelagem Ecológica e Ciência de Dados para Biodiversidade

GitHub: [@rogerio-onza](https://github.com/rogerio-onza)
