package cn.blmdz.wolf.search.impl;

import java.util.List;
import java.util.Map;

import cn.blmdz.home.common.model.PageInfo;
import cn.blmdz.home.search.api.query.Aggs;
import cn.blmdz.home.search.api.query.Criterias;
import cn.blmdz.home.search.api.query.CriteriasBuilder;
import cn.blmdz.home.search.api.query.Highlight;
import cn.blmdz.home.search.api.query.Keyword;
import cn.blmdz.home.search.api.query.Range;
import cn.blmdz.home.search.api.query.Sort;
import cn.blmdz.home.search.api.query.Term;
import cn.blmdz.home.search.api.query.Terms;

public abstract class BaseItemQueryBuilder {
	public Criterias makeCriterias(Integer pageNo, Integer size, Map<String, String> params) {
		CriteriasBuilder criteriasBuilder = new CriteriasBuilder();

		PageInfo pageInfo = new PageInfo(pageNo, size);
		criteriasBuilder.withPageInfo(pageInfo.getOffset().intValue(), pageInfo.getLimit().intValue());

		Keyword keyword = buildKeyword(params);

		criteriasBuilder.withKeyword(keyword);

		List termList = buildTerm(params);

		criteriasBuilder.withTerm(termList);

		List termsList = buildTerms(params);
		criteriasBuilder.withTerms(termsList);

		List ranges = buildRanges(params);
		criteriasBuilder.withRanges(ranges);

		List sorts = buildSort(params);

		criteriasBuilder.withSorts(sorts);

		List aggsList = buildAggs(params);

		List highlightList = buildHighlight(params);

		criteriasBuilder.withHighlights(highlightList);

		criteriasBuilder.withAggs(aggsList);
		return criteriasBuilder.build();
	}

	public abstract Keyword buildKeyword(Map<String, String> paramMap);

	public abstract List<Term> buildTerm(Map<String, String> paramMap);

	public abstract List<Terms> buildTerms(Map<String, String> paramMap);

	public abstract List<Range> buildRanges(Map<String, String> paramMap);

	public abstract List<Aggs> buildAggs(Map<String, String> paramMap);

	public abstract List<Sort> buildSort(Map<String, String> paramMap);

	public abstract List<Highlight> buildHighlight(Map<String, String> paramMap);
}