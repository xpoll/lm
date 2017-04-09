package cn.blmdz.wolf.search.dto;

import java.io.Serializable;

import cn.blmdz.home.common.model.Paging;

public class SearchedShopWithAggs implements Serializable {
	private static final long serialVersionUID = 2687076924260837480L;
	private Paging<SearchedShop> entities;

	public void setEntities(Paging<SearchedShop> entities) {
		this.entities = entities;
	}

	public Paging<SearchedShop> getEntities() {
		return this.entities;
	}
}