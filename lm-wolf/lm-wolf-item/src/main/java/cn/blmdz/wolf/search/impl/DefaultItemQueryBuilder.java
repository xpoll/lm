package cn.blmdz.wolf.search.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import com.google.common.base.Splitter;
import com.google.common.base.Strings;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.home.search.api.query.Aggs;
import cn.blmdz.home.search.api.query.Highlight;
import cn.blmdz.home.search.api.query.Keyword;
import cn.blmdz.home.search.api.query.Range;
import cn.blmdz.home.search.api.query.Sort;
import cn.blmdz.home.search.api.query.Term;
import cn.blmdz.home.search.api.query.Terms;

public class DefaultItemQueryBuilder extends BaseItemQueryBuilder {
	private static final Logger log = LoggerFactory.getLogger(DefaultItemQueryBuilder.class);

	private static final Splitter DOLLAR_SPLITTER = Splitter.on('$').omitEmptyStrings().trimResults();

	public Keyword buildKeyword(Map<String, String> params) {
		String q = (String) params.get("q");
		if (StringUtils.hasText(q)) {
			return new Keyword(ImmutableList.of("name", "brandName"), q);
		}
		return null;
	}

	public List<Term> buildTerm(Map<String, String> params) {
		List termList = Lists.newArrayList();
		String bid = (String) params.get("bid");
		if (StringUtils.hasText(bid)) {
			termList.add(new Term("brandId", bid));
		}

		String shopId = (String) params.get("shopId");
		if (StringUtils.hasText(shopId)) {
			termList.add(new Term("shopId", shopId));
		}

		String shopCatId = (String) params.get("shopCatId");
		if (StringUtils.hasText(shopCatId)) {
			termList.add(new Term("shopCategoryId", shopCatId));
		}
		return termList;
	}

	public List<Terms> buildTerms(Map<String, String> params) {
	      List<Terms> termsList = Lists.newArrayList();
	      String attributes = (String)params.get("attrs");
	      if(StringUtils.hasText(attributes)) {
	         List<String> parts = Splitters.UNDERSCORE.splitToList(attributes);
	         ArrayListMultimap<String, String> groupByKey = ArrayListMultimap.create();

	         for(String part : parts) {
	            List<String> kv = Splitters.COLON.splitToList(part);
	            if(kv.size() != 2) {
	               log.warn("kv not split by `:\', kv={}", kv);
	            } else {
	               groupByKey.put(kv.get(0), part);
	            }
	         }

	         for(String key : groupByKey.keySet()) {
	            List<String> kvs = groupByKey.get(key);
	            if(!kvs.isEmpty()) {
	               termsList.add(new Terms("attributes", new ArrayList(kvs)));
	            }
	         }
	      }

	      String bids = (String)params.get("bids");
	      if(StringUtils.hasText(bids)) {
	         List<String> parts = Splitters.UNDERSCORE.splitToList(bids);
	         termsList.add(new Terms("brandId", parts));
	      }

	      String regionIds = (String)params.get("regionIds");
	      if(StringUtils.hasText(regionIds)) {
	         termsList.add(new Terms("regionIds", Splitters.UNDERSCORE.splitToList(regionIds)));
	      }

	      String ids = (String)params.get("ids");
	      if(StringUtils.hasText(ids)) {
	         termsList.add(new Terms("id", Splitters.UNDERSCORE.splitToList(ids)));
	      }

	      String bcids = (String)params.get("bcids");
	      if(StringUtils.hasText(bcids)) {
	         termsList.add(new Terms("categoryIds", Splitters.UNDERSCORE.splitToList(bcids)));
	      }

	      return termsList;
	   }

	public List<Range> buildRanges(Map<String, String> params) {
		List ranges = Lists.newArrayList();

		String p_f = (String) params.get("p_f");
		String p_t = (String) params.get("p_t");
		if ((StringUtils.hasText(p_f)) || (StringUtils.hasText(p_t))) {
			Range lowPrice = new Range("price", p_f, p_t);
			ranges.add(lowPrice);
		}
		return ranges;
	}

	public List<Aggs> buildAggs(Map<String, String> params) {
		String aggs = (String) params.get("aggs");
		if (StringUtils.hasText(aggs)) {
			List<String> aggSpecifiers = DOLLAR_SPLITTER.splitToList(aggs);
			List result = Lists.newArrayListWithCapacity(aggSpecifiers.size());
			for (String aggSpecifier : aggSpecifiers) {
				List parts = Splitters.COLON.splitToList(aggSpecifier);
				Aggs agg = new Aggs((String) parts.get(0), (String) parts.get(1),
						Integer.valueOf(Integer.parseInt((String) parts.get(2))));
				result.add(agg);
			}
			return result;
		}
		return null;
	}

	public List<Sort> buildSort(Map<String, String> params) {
		List sorts = Lists.newArrayList();
		String sort = (String) params.get("sort");
		if (!Strings.isNullOrEmpty(sort)) {
			List parts = Splitters.UNDERSCORE.splitToList(sort);
			if (parts.size() < 4) {
				return Collections.emptyList();
			}
			String price = (String) Iterables.getFirst(parts, "0");
			String stockQuantity = (String) Iterables.get(parts, 1, "0");
			String saleQuantity = (String) Iterables.get(parts, 2, "0");
			String createdAt = (String) Iterables.get(parts, 3, "0");
			switch (Integer.parseInt(price)) {
			case 1:
				sorts.add(new Sort("price", "asc"));
				break;
			case 2:
				sorts.add(new Sort("price", "desc"));
			}

			switch (Integer.parseInt(stockQuantity)) {
			case 1:
				sorts.add(new Sort("stockQuantity", "asc"));
				break;
			case 2:
				sorts.add(new Sort("stockQuantity", "desc"));
			}

			switch (Integer.parseInt(saleQuantity)) {
			case 1:
				sorts.add(new Sort("saleQuantity", "asc"));
				break;
			case 2:
				sorts.add(new Sort("saleQuantity", "desc"));
			}

			switch (Integer.parseInt(createdAt)) {
			case 1:
				sorts.add(new Sort("createdAt", "asc"));
				break;
			case 2:
				sorts.add(new Sort("createdAt", "desc"));
			}

		}

		return sorts;
	}

	public List<Highlight> buildHighlight(Map<String, String> params) {
		String highlight = (String) params.get("highlight");
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