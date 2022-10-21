import Distribution.Simple
import System.Process

main = do
  createProcess (shell
    "lhs2TeX src/QueueFamilySelect.lhs > QueueFamilySelect.tex && texi2pdf -c -q QueueFamilySelect.tex")
  createProcess (shell
    "pandoc --from=LaTeX+lhs --to=plain src/QueueFamilySelect.lhs -o QueueFamilySelect.txt")
  createProcess (shell
    "pandoc --from=LaTeX+lhs --to=plain test/Spec.lhs -o Spec.txt")
  defaultMain