import React from 'react';
import styled from '@emotion/styled';
import { css, Theme } from '@emotion/react';
import { Link } from 'react-router-dom';

import Logo from '~/components/Logo';
import SyntaxHighlighter from '~/components/SyntaxHighlighter';
import { CheckIcon, CopyIcon, GithubIcon, PlayIcon } from '~/components/Icons';
import { useT, MessageId } from '~/translations';
import { docs } from '~/utils/docs';
import { ABILITY_VERSION, GITHUB_URL } from '~/utils/env';

const INSTALL_COMMAND = 'npm install @via-profit/ability';

const EXAMPLE_DSL = `@name "Author can update a draft order"
permit permission.order.update if all:
  order.authorId equals user.id
  order.status in ['draft', 'review']

@name "Completed orders are read-only, except for admins"
deny permission.order.update if all:
  order.status equals 'completed'
  except any of:
    user.roles contains 'admin'`;

const EXAMPLE_CODE = `import {
  AbilityDSLParser,
  AbilityResolver,
  DenyOverridesStrategy,
} from '@via-profit/ability';

const policies = new AbilityDSLParser(dsl).parse();
const resolver = new AbilityResolver(policies, DenyOverridesStrategy);

// Throws AbilityError if access is denied
resolver.enforce('order.update', { order, user });

// Or check manually and explain the decision
const result = resolver.resolve('order.update', { order, user });
if (result.isDenied()) {
  console.log(result.explainToString());
}`;

const gradient = (theme: Theme) =>
  `linear-gradient(90deg, ${theme.color.accentSecondary.toString()}, ${theme.color.accentPrimary.toString()})`;

const Hero = styled.section`
  position: relative;
  padding: 6rem 1.5rem 4rem;
  text-align: center;
  overflow: hidden;

  &::before {
    content: '';
    position: absolute;
    top: -12rem;
    left: 50%;
    width: 48rem;
    max-width: 100%;
    height: 28rem;
    transform: translateX(-50%);
    background: radial-gradient(
      closest-side,
      ${({ theme }) => theme.color.accentPrimary.alpha(theme.isDark ? 0.18 : 0.14).toString()},
      transparent
    );
    pointer-events: none;
  }

  @media all and (max-width: 640px) {
    padding: 3.5rem 1rem 3rem;
  }
`;

const Pill = styled.span`
  position: relative;
  display: inline-block;
  padding: 0.3rem 0.8rem;
  font-family: var(--font-mono);
  font-size: 0.75rem;
  color: ${({ theme }) => theme.color.accentPrimary.toString()};
  border: 1px solid ${({ theme }) => theme.color.accentPrimary.alpha(0.35).toString()};
  border-radius: 999px;
  background-color: ${({ theme }) => theme.color.accentPrimary.alpha(0.08).toString()};
`;

const Title = styled.h1`
  position: relative;
  max-width: 46rem;
  margin: 1.5rem auto 1rem;
  font-size: clamp(2.2rem, 5vw, 3.6rem);
  font-weight: 800;
  line-height: 1.1;
  letter-spacing: -0.03em;
`;

const TitleAccent = styled.span`
  background: ${({ theme }) => gradient(theme)};
  -webkit-background-clip: text;
  background-clip: text;
  color: transparent;
`;

const Lead = styled.p`
  position: relative;
  max-width: 38rem;
  margin: 0 auto 2.25rem;
  font-size: 1.1rem;
  line-height: 1.7;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
`;

const Actions = styled.div`
  position: relative;
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: 0.75rem;
`;

const buttonStyles = css`
  display: inline-flex;
  align-items: center;
  gap: 0.5rem;
  height: 2.75rem;
  padding: 0 1.4rem;
  font-size: 0.95rem;
  font-weight: 600;
  text-decoration: none;
  border-radius: 0.6rem;
  transition:
    transform 120ms ease-out,
    box-shadow 120ms ease-out,
    border-color 120ms ease-out;

  &:hover {
    transform: translateY(-1px);
  }
`;

const PrimaryButton = styled(Link)`
  ${buttonStyles};
  color: #ffffff;
  background: ${({ theme }) => gradient(theme)};
  box-shadow: 0 0.5rem 1.5rem -0.5rem ${({ theme }) => theme.color.accentPrimary.alpha(0.6).toString()};

  &:hover {
    box-shadow: 0 0.75rem 2rem -0.5rem ${({ theme }) => theme.color.accentPrimary.alpha(0.8).toString()};
  }
`;

const secondaryStyles = (theme: Theme) => css`
  ${buttonStyles};
  color: ${theme.color.textPrimary.toString()};
  border: 1px solid ${theme.color.border.toString()};
  background-color: ${theme.color.surface.toString()};

  &:hover {
    border-color: ${theme.color.accentPrimary.alpha(0.5).toString()};
  }
`;

const SecondaryButton = styled(Link)`
  ${({ theme }) => secondaryStyles(theme)};
`;

const SecondaryAnchor = styled.a`
  ${({ theme }) => secondaryStyles(theme)};
`;

const Install = styled.button`
  position: relative;
  display: inline-flex;
  align-items: center;
  gap: 0.75rem;
  margin-top: 2.5rem;
  padding: 0.65rem 1rem;
  font-family: var(--font-mono);
  font-size: 0.85rem;
  color: ${({ theme }) => theme.color.textPrimary.toString()};
  background-color: ${({ theme }) => theme.color.codeBackground.toString()};
  border: 1px solid ${({ theme }) => theme.color.border.toString()};
  border-radius: 0.6rem;
  cursor: pointer;

  &::before {
    content: '$';
    color: ${({ theme }) => theme.color.accentPrimary.toString()};
  }

  & svg {
    color: ${({ theme }) => theme.color.textSecondary.toString()};
  }

  &:hover {
    border-color: ${({ theme }) => theme.color.accentPrimary.alpha(0.5).toString()};
  }
`;

const PackageName = styled(Logo)`
  position: relative;
  margin-top: 3rem;
  font-size: 1.6rem;

  @media all and (max-width: 640px) {
    font-size: 1.1rem;
  }
`;

const Section = styled.section`
  width: 100%;
  max-width: 72rem;
  margin: 0 auto;
  padding: 1rem 1.5rem 4.5rem;

  @media all and (max-width: 640px) {
    padding: 1rem 1rem 3rem;
  }
`;

const SectionTitle = styled.h2`
  margin: 0 0 1.5rem;
  font-size: 1.35rem;
  font-weight: 600;
  letter-spacing: -0.01em;
`;

const ExampleGrid = styled.div`
  display: grid;
  grid-template-columns: minmax(0, 1fr) minmax(0, 1.15fr);
  gap: 1.25rem;

  @media all and (max-width: 960px) {
    grid-template-columns: minmax(0, 1fr);
  }

  & > div > div {
    margin: 0;
  }
`;

const ExampleLabel = styled.div`
  margin-bottom: 0.5rem;
  font-size: 0.7rem;
  font-weight: 600;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
`;

const FeatureGrid = styled.div`
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(18rem, 1fr));
  gap: 0.75rem;
`;

const Feature = styled.div`
  padding: 1.25rem 1.25rem 1.1rem;
  background-color: ${({ theme }) => theme.color.surface.toString()};
  border: 1px solid ${({ theme }) => theme.color.border.toString()};
  border-radius: 0.75rem;
`;

const FeatureTitle = styled.h3`
  margin: 0 0 0.4rem;
  font-size: 1rem;
  font-weight: 600;

  &::before {
    content: '';
    display: inline-block;
    width: 0.5rem;
    height: 0.5rem;
    margin-right: 0.6rem;
    vertical-align: middle;
    border-radius: 50%;
    background: ${({ theme }) => gradient(theme)};
    box-shadow: 0 0 10px ${({ theme }) => theme.color.accentPrimary.alpha(0.7).toString()};
  }
`;

const FeatureText = styled.p`
  margin: 0;
  font-size: 0.9rem;
  line-height: 1.6;
  color: ${({ theme }) => theme.color.textSecondary.toString()};
`;

const CardGrid = styled.div`
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(14rem, 1fr));
  gap: 0.75rem;
`;

const Card = styled(Link)`
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 0.5rem;
  padding: 1rem 1.1rem;
  font-size: 0.925rem;
  font-weight: 500;
  color: ${({ theme }) => theme.color.textPrimary.toString()};
  text-decoration: none;
  background-color: ${({ theme }) => theme.color.surface.toString()};
  border: 1px solid ${({ theme }) => theme.color.border.toString()};
  border-radius: 0.6rem;
  transition:
    border-color 120ms ease-out,
    box-shadow 120ms ease-out;

  &::after {
    content: '→';
    color: ${({ theme }) => theme.color.textSecondary.toString()};
    transition:
      color 120ms ease-out,
      transform 120ms ease-out;
  }

  &:hover {
    border-color: ${({ theme }) => theme.color.accentPrimary.alpha(0.6).toString()};
    box-shadow:
      0 0 0 1px ${({ theme }) => theme.color.accentPrimary.alpha(0.2).toString()},
      0 0.5rem 1.5rem -0.75rem ${({ theme }) => theme.color.accentPrimary.alpha(0.5).toString()};
  }

  &:hover::after {
    color: ${({ theme }) => theme.color.accentPrimary.toString()};
    transform: translateX(2px);
  }
`;

const features: readonly { readonly title: MessageId; readonly text: MessageId }[] = [
  { title: 'home.feature.dsl.title', text: 'home.feature.dsl.text' },
  { title: 'home.feature.types.title', text: 'home.feature.types.text' },
  { title: 'home.feature.strategies.title', text: 'home.feature.strategies.text' },
  { title: 'home.feature.explain.title', text: 'home.feature.explain.text' },
  { title: 'home.feature.isomorphic.title', text: 'home.feature.isomorphic.text' },
  { title: 'home.feature.fast.title', text: 'home.feature.fast.text' },
];

const Home: React.FC = () => {
  const t = useT();
  const [copied, setCopied] = React.useState(false);

  React.useEffect(() => {
    document.title = '@via-profit/ability';
  }, []);

  React.useEffect(() => {
    if (!copied) {
      return undefined;
    }
    const timeout = setTimeout(() => setCopied(false), 1500);

    return () => clearTimeout(timeout);
  }, [copied]);

  const copyInstall = () => {
    navigator.clipboard
      ?.writeText(INSTALL_COMMAND)
      .then(() => setCopied(true))
      .catch(() => undefined);
  };

  return (
    <>
      <Hero>
        <Pill>
          {ABILITY_VERSION && `v${ABILITY_VERSION} · `}
          {t('home.pill')}
        </Pill>
        <Title>
          {t('home.title')} <TitleAccent>{t('home.titleAccent')}</TitleAccent>
        </Title>
        <Lead>{t('home.lead')}</Lead>
        <Actions>
          <PrimaryButton to="/docs">{t('home.docs')}</PrimaryButton>
          <SecondaryButton to="/playground">
            <PlayIcon />
            {t('home.playground')}
          </SecondaryButton>
          <SecondaryAnchor href={GITHUB_URL} target="_blank" rel="noopener noreferrer">
            <GithubIcon />
            GitHub
          </SecondaryAnchor>
        </Actions>
        <div>
          <Install type="button" onClick={copyInstall} title={t('code.copy')}>
            {INSTALL_COMMAND}
            {copied ? <CheckIcon /> : <CopyIcon />}
          </Install>
        </div>
        <div>
          <PackageName />
        </div>
      </Hero>

      <Section>
        <SectionTitle>{t('home.example')}</SectionTitle>
        <ExampleGrid>
          <div>
            <ExampleLabel>{t('home.exampleDsl')}</ExampleLabel>
            <SyntaxHighlighter language="dsl" code={EXAMPLE_DSL} />
          </div>
          <div>
            <ExampleLabel>{t('home.exampleCode')}</ExampleLabel>
            <SyntaxHighlighter language="ts" code={EXAMPLE_CODE} />
          </div>
        </ExampleGrid>
      </Section>

      <Section>
        <SectionTitle>{t('home.features')}</SectionTitle>
        <FeatureGrid>
          {features.map(feature => (
            <Feature key={feature.title}>
              <FeatureTitle>{t(feature.title)}</FeatureTitle>
              <FeatureText>{t(feature.text)}</FeatureText>
            </Feature>
          ))}
        </FeatureGrid>
      </Section>

      <Section>
        <SectionTitle>{t('nav.docs')}</SectionTitle>
        <CardGrid>
          {docs.map(doc => (
            <Card key={doc.id} to={doc.path}>
              {t(doc.title)}
            </Card>
          ))}
        </CardGrid>
      </Section>
    </>
  );
};

export default Home;
