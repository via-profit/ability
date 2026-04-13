import { AnnotationName } from "./AbilityDSLAnnotations";

export const AnnotationAllowed: Record<
  "policy" | "ruleSet" | "rule" | "alias",
  Set<AnnotationName>
> = {
  policy: new Set(["id", "name", "description", "priority", "disabled", "tags"]),
  ruleSet: new Set(["id", "name", "description", "disabled"]),
  rule: new Set(["id", "name", "disabled"]),
  alias: new Set(["name", "disabled"]),
};
