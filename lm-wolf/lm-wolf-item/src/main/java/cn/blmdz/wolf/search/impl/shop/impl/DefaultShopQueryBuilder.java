package cn.blmdz.wolf.search.impl.shop.impl;

import java.util.List;
import java.util.Map;

import org.springframework.util.StringUtils;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.home.search.api.query.Aggs;
import cn.blmdz.home.search.api.query.Highlight;
import cn.blmdz.home.search.api.query.Keyword;
import cn.blmdz.home.search.api.query.Range;
import cn.blmdz.home.search.api.query.Sort;
import cn.blmdz.home.search.api.query.Term;
import cn.blmdz.home.search.api.query.Terms;

public class DefaultShopQueryBuilder extends BaseShopQueryBuilder
{
  public Keyword buildKeyword(Map<String, String> params)
  {
    String q = (String)params.get("q");
    if (StringUtils.hasText(q)) {
      return new Keyword(ImmutableList.of("name"), q);
    }
    return null;
  }

  public List<Term> buildTerm(Map<String, String> params)
  {
    return null;
  }

  public List<Terms> buildTerms(Map<String, String> params)
  {
    return null;
  }

  public List<Range> buildRanges(Map<String, String> params)
  {
    return null;
  }

  public List<Aggs> buildAggs(Map<String, String> params)
  {
    return null;
  }

  public List<Sort> buildSort(Map<String, String> params)
  {
    return null;
  }

  public List<Highlight> buildHighlight(Map<String, String> params)
  {
    String highlight = (String)params.get("highlight");
    if (StringUtils.hasText(highlight)) {
      List<String> fields = Splitters.UNDERSCORE.splitToList(highlight);
      List highlights = Lists.newArrayListWithCapacity(fields.size());
      for (String field : fields) {
        highlights.add(new Highlight(field));
      }
      return highlights;
    }
    return null;
  }
}