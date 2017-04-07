package cn.blmdz.wolf.parana.search.dto;

import java.io.Serializable;
import java.util.List;

import cn.blmdz.home.common.model.Paging;

public class SearchedItemWithAggs implements Serializable {
	private static final long serialVersionUID = 8702350543499146460L;
	private Paging<SearchedItem> entities;
	private List<GroupedAggNav> attributes;
	private List<AggNav> brands;
	private List<AggNav> backCategories;
	private List<IdAndName> breadCrumbs;
	private List<Chosen> chosen;

	public Paging<SearchedItem> getEntities() {
		return this.entities;
	}

	public List<GroupedAggNav> getAttributes() {
		return this.attributes;
	}

	public List<AggNav> getBrands() {
		return this.brands;
	}

	public List<AggNav> getBackCategories() {
		return this.backCategories;
	}

	public List<IdAndName> getBreadCrumbs() {
		return this.breadCrumbs;
	}

	public List<Chosen> getChosen() {
		return this.chosen;
	}

	public void setEntities(Paging<SearchedItem> entities) {
		this.entities = entities;
	}

	public void setAttributes(List<GroupedAggNav> attributes) {
		this.attributes = attributes;
	}

	public void setBrands(List<AggNav> brands) {
		this.brands = brands;
	}

	public void setBackCategories(List<AggNav> backCategories) {
		this.backCategories = backCategories;
	}

	public void setBreadCrumbs(List<IdAndName> breadCrumbs) {
		this.breadCrumbs = breadCrumbs;
	}

	public void setChosen(List<Chosen> chosen) {
		this.chosen = chosen;
	}

}