export declare const optionSchemas: {
    n_rows: {
        type: string;
        minimum: number;
        description: string;
    };
    show_conditions: {
        type: string;
        items: {
            enum: string[];
        };
        uniqueItems: true;
        description: string;
    };
    dark_mode: {
        enum: string[];
        description: string;
    };
};
