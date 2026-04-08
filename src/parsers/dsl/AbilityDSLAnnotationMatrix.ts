import { AnnotationName } from "./AbilityDSLAnnotations";

export const AnnotationAllowed: Record<
  "policy" | "ruleSet" | "rule",
  Set<AnnotationName>
> = {
  policy: new Set(["id", "name", "description", "priority", "disabled", "tags"]),
  ruleSet: new Set(["id", "name", "description", "disabled"]),
  rule: new Set(["id", "name", "disabled"]),
};
