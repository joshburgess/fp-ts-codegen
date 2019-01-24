import * as P from 'parser-ts'
import * as S from 'parser-ts/lib/string'
import * as C from 'parser-ts/lib/char'
import * as M from './model'
import { Either } from 'fp-ts/lib/Either'
import { some } from 'fp-ts/lib/Option'
import { tuple } from 'fp-ts/lib/function'
import { Tree, Forest } from 'fp-ts/lib/Tree'

const isDigit = (c: string): boolean => '0123456789'.indexOf(c) !== -1

const isPunctuation = (c: string): boolean => '| =\n():,{};[]'.indexOf(c) !== -1

const identifierFirstLetter = P.sat(c => !isDigit(c) && !isPunctuation(c))

const identifierBody = P.sat(c => !isPunctuation(c))

const expected = <A>(message: string, parser: P.Parser<A>): P.Parser<A> =>
  P.expectedL(parser, remaining => `Expected ${message}, cannot parse ${JSON.stringify(remaining)}`)

export const identifier: P.Parser<string> = expected(
  'an identifier',
  P.fold([identifierFirstLetter, C.many(identifierBody)])
)

const leftParens: P.Parser<string> = P.fold([C.char('('), S.spaces])

const rightParens: P.Parser<string> = P.fold([S.spaces, C.char(')')])

export const getTreeParser = <A>(parser: P.Parser<A>): P.Parser<Tree<A>> => {
  const valueWithParens: P.Parser<A> = leftParens.applySecond(parser).applyFirst(rightParens)
  const value: P.Parser<A> = valueWithParens.alt(parser)
  const tree: P.Parser<Tree<A>> = value.chain(value => forest.map(forest => new Tree(value, forest)))
  const forest: P.Parser<Forest<A>> = S.spaces.chain(() => P.sepBy(S.spaces, treeWithParens.alt(leaf)))
  const leaf: P.Parser<Tree<A>> = parser.map(value => new Tree(value, []))
  const treeWithParens: P.Parser<Tree<A>> = leftParens.applySecond(tree).applyFirst(rightParens)
  return tree
}

const tree: P.Parser<Tree<string>> = getTreeParser(identifier)

const getTypeReference = (tree: Tree<string>): M.Type => {
  return M.typeReference(tree.value, tree.forest.map(getTypeReference))
}

const typeReference: P.Parser<M.Type> = tree.map(getTypeReference)

const comma = P.fold([S.spaces, C.char(','), S.spaces])

const other: P.Parser<Array<M.Type>> = comma.chain(() => P.sepBy(comma, type)).alt(P.parser.of([]))

const tupleType: P.Parser<M.Type> = leftParens
  .chain(() => type.applyFirst(comma).chain(fst => type.chain(snd => other.map(other => M.tupleType(fst, snd, other)))))
  .applyFirst(rightParens)

export const type: P.Parser<M.Type> = typeReference.alt(tupleType)

const pair: P.Parser<[string, M.Type]> = identifier.chain(name =>
  P.fold([S.spaces, S.string('::'), S.spaces])
    .applySecond(type)
    .map(type => tuple(name, type))
)

const objectType: P.Parser<Array<[string, M.Type]>> = P.fold([C.char('{'), S.spaces])
  .applySecond(P.sepBy(comma, pair))
  .applyFirst(P.fold([S.spaces, C.char('}')]))

const recordConstructor: P.Parser<M.Constructor> = identifier.chain(name =>
  S.spaces.applySecond(
    objectType.map(pairs => M.constructor(name, pairs.map(([name, type]) => M.member(type, some(name)))))
  )
)

const getPositionalConstructor = (tree: Tree<string>): M.Constructor => {
  return M.constructor(tree.value, tree.forest.map(member => M.member(getTypeReference(member))))
}

const positionalConstructor: P.Parser<M.Constructor> = tree.map(getPositionalConstructor)

export const constructor: P.Parser<M.Constructor> = recordConstructor.alt(positionalConstructor)

const equal = P.fold([S.spaces, C.char('='), S.spaces])

const parameterWithoutConstraint: P.Parser<M.Parameter> = identifier.map(name => M.parameter(name))

const parameterWithConstraint: P.Parser<M.Parameter> = P.fold([C.char('('), S.spaces]).applySecond(
  pair.map(([name, type]) => M.parameter(name, some(type))).applyFirst(P.fold([S.spaces, C.char(')')]))
)

export const parameter = expected('a parameter', parameterWithoutConstraint.alt(parameterWithConstraint))

export const introduction: P.Parser<M.Introduction> = expected(
  'a data type declaration',
  S.string('data').chain(() =>
    S.spaces.applySecond(
      identifier.chain(name =>
        S.spaces
          .applySecond(P.sepBy(S.spaces, parameter))
          .map(parameters => M.introduction(name, parameters))
          .applyFirst(equal)
      )
    )
  )
)

const pipe = P.fold([S.spaces, C.char('|'), S.spaces])

export const data: P.Parser<M.Data> = expected(
  'a data declaration',
  introduction
    .chain(definition =>
      P.sepBy1(pipe, constructor).map(constructors => M.data(definition, constructors.head, constructors.tail))
    )
    .applyFirst(S.spaces)
    .applyFirst(P.eof)
)

export const parse = (s: string): Either<string, M.Data> => {
  return data.run(s).bimap(e => e.message, ([data]) => data)
}
