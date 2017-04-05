package io.terminus.parana.search.impl;

import io.terminus.common.model.PageInfo;
import io.terminus.search.api.query.Aggs;
import io.terminus.search.api.query.Criterias;
import io.terminus.search.api.query.CriteriasBuilder;
import io.terminus.search.api.query.Highlight;
import io.terminus.search.api.query.Keyword;
import io.terminus.search.api.query.Range;
import io.terminus.search.api.query.Sort;
import io.terminus.search.api.query.Term;
import io.terminus.search.api.query.Terms;
import java.util.List;
import java.util.Map;

public abstract class BaseItemQueryBuilder {
   public Criterias makeCriterias(Integer pageNo, Integer size, Map params) {
      CriteriasBuilder criteriasBuilder = new CriteriasBuilder();
      PageInfo pageInfo = new PageInfo(pageNo, size);
      criteriasBuilder.withPageInfo(pageInfo.getOffset().intValue(), pageInfo.getLimit().intValue());
      Keyword keyword = this.buildKeyword(params);
      criteriasBuilder.withKeyword(keyword);
      List<Term> termList = this.buildTerm(params);
      criteriasBuilder.withTerm(termList);
      List<Terms> termsList = this.buildTerms(params);
      criteriasBuilder.withTerms(termsList);
      List<Range> ranges = this.buildRanges(params);
      criteriasBuilder.withRanges(ranges);
      List<Sort> sorts = this.buildSort(params);
      criteriasBuilder.withSorts(sorts);
      List<Aggs> aggsList = this.buildAggs(params);
      List<Highlight> highlightList = this.buildHighlight(params);
      criteriasBuilder.withHighlights(highlightList);
      criteriasBuilder.withAggs(aggsList);
      return criteriasBuilder.build();
   }

   public abstract Keyword buildKeyword(Map var1);

   public abstract List buildTerm(Map var1);

   public abstract List buildTerms(Map var1);

   public abstract List buildRanges(Map var1);

   public abstract List buildAggs(Map var1);

   public abstract List buildSort(Map var1);

   public abstract List buildHighlight(Map var1);
}
