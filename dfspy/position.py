
class Positions(object):
  """Player position constants."""

  QB = 'QB'
  RB = 'RB'
  WR = 'WR'
  TE = 'TE'
  DEF = 'DST'

  @classmethod
  def all(cls):
    """All positions returned as tuple."""
    return (cls.QB, cls.RB, cls.WR, cls.TE, cls.DEF)

  @classmethod
  def num_required(cls, position):
    """Number of active players required for position."""
    return {
      cls.QB: 1,
      cls.RB: 2,
      cls.WR: 3,
      cls.TE: 1,
      cls.DEF: 1,
    }[position]

  @classmethod
  def is_flex(cls, position):
    """Whether the position is a flex position."""
    return position in {cls.RB, cls.WR, cls.TE}

if __name__ == '__main__':
  print Positions.all()