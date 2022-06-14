(**
  Verify quota computations.

  Quotas are meant to safeguard against guest actions exhausting memory
  and causing the process to be OOM killed.
  They ensure that memory used by data structures is bounded.

  It can be difficult to accurately predict the amount of memory used when given certain input:
  it depends on data structure implementation details and the garbage collector.
  Therefore size computations and quotas are only approximate, however we need to test whether they are correct,
  and thus need a mathematical formula on what is acceptable in this approximation.

  The approximation used in oxenstored can be expressed by the following property:
    [∀Input, ∃ Bound < AvailableMemory: memory_used_bytes(Input) < Bound]

  where:
   * [Input] is a sequence of guest actions (e.g. xenstore packets), potentially unbounded
   * [memory_used_bytes(x)] is the amount of memory used when supplied the input [x]
   * [Bound] is the upper bound ensured by the quota system
   * [AvailableMemory] is the amount of memory the process is allowed to allocate
    before getting OOM killed. E.g. if OOM score adjust is set to 50% fo RAM, then 50% of RAM.

  The application is allowed to use less memory than the computed Bound, either
  due to optimization (e.g. sharing), or to account for inaccuracies in bound computation.
  However [Bound] is the upper bound used to ensure OOM safety.

  The condition [Bound < AvailableMemory] is important, otherwise [Bound = 2^64]
  would always be a valid value (memory allocation will have to fail once that is exceeded,
  because pointers cannot represent more than 64-bit values), but such a bound
  is not useful in practice: the application would've been OOM killed long before that.

  However the following property is desireable:
  [∀ Bound ≥ Baseline, ∃ Quota_i ≥ 0, Constant_i, Constant_i ≥ 1, Bound = Baseline + ∑ Quota_i * Constant_i]

  where:
  * [Baseline] is the baseline memory usage of oxenstored including any one-off
  memory usage that stays ~constant for its lifetime (e.g. the executable itself,
  initial heap size, and initial size of global data structures)
  Any one-off overhead from datastructures can also be included here as long as there is another quota
  limiting the number of such data structures, thus ensuring that any overhead has a constant upper bound

  * [Quota_i] is a user supplied configuration value, set to achieve the desired [AvailableMemory]
  * [Constant_i] is a constant factor to ensure linear memory usage.
  Logarithmic memory usage (e.g. for tree depth) can be accounted for by introducing a quota on the number of items,
  and then adding the maximum value of that as a constant here.
  The application is of course allowed to optimize memory usage (e.g. via sharing) and use less memory
  than computed here, but that is already allowed by the initial equation.
  A small [Constant_i] is desireable, however an upper bound on this is eventually ensured by
  [Bound < AvailableMemory]

  * [∑] accounts for needing multiple quotas for different kinds of input

  There are similarities between this property and the Big-O notation used for
  algorithmic and space complexity, except we're not interested only in what happens
  when the input goes to infinity, we're also interested whether there are any
  large constant factors.

*)
